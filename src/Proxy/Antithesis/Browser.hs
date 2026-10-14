{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Module: Proxy.Antithesis.Browser
-- Description: Browser sign-in and authorized disclosure of report capabilities.
-- Copyright: (c) Cardano Foundation
-- License: Apache-2.0
--
-- Browser state and OAuth credentials stay on the server. Browser cookies never
-- enter the bearer API gate. All external failures are reduced to safe messages.
module Proxy.Antithesis.Browser
    ( BrowserDependencies (..)
    , ReportResult (..)
    , browserApplication
    , browserDependencies
    , privateResponse
    ) where

import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar)
import Control.Exception (SomeException, try)
import Control.Monad (guard)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import Data.Aeson
    ( FromJSON (..)
    , eitherDecode
    , withObject
    , (.!=)
    , (.:)
    , (.:?)
    )
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock (UTCTime, addUTCTime)
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Lib.GitHub.Auth.Identify (GitHubError (..), Login)
import Lib.GitHub.Auth.TeamCheck (MembershipResult (..))
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types
    ( HeaderName
    , Query
    , ResponseHeaders
    , Status
    , hAccept
    , hAuthorization
    , hCacheControl
    , hContentLength
    , hContentType
    , hCookie
    , hLocation
    , parseQuery
    , renderQuery
    , status200
    , status303
    , status400
    , status401
    , status403
    , status404
    , status405
    , status409
    , status502
    , status503
    , statusCode
    )
import Network.HTTP.Types.Header (hOrigin, hSetCookie)
import Network.Wai
    ( Application
    , Request (pathInfo, queryString, requestHeaders, requestMethod)
    , Response
    , responseLBS
    , strictRequestBody
    )
import Proxy.Antithesis.Config (BrowserSettings (..))
import Proxy.Antithesis.Middleware.Auth (AuthConfig (..))
import Proxy.Antithesis.Server (UpstreamConfig (..))
import Servant.Client (showBaseUrl)

-- | Explicit effect boundaries, also used with real loopback HTTP upstreams.
data BrowserDependencies = BrowserDependencies
    { browserNow :: IO UTCTime
    , browserRandom :: IO ByteString
    , browserExchange
        :: BrowserSettings -> ByteString -> ByteString -> IO (Maybe OAuthToken)
    , browserIdentify :: OAuthToken -> IO (Either GitHubError Login)
    , browserMembership :: OAuthToken -> Login -> IO MembershipResult
    , browserReport :: Text -> IO ReportResult
    }

-- | Safe outcomes; raw provider bodies never cross this boundary.
data ReportResult
    = ReportReady ByteString
    | ReportPending
    | ReportMissing
    | ReportUnavailable

data LoginState = LoginState
    { loginExpires :: UTCTime
    , loginBinding :: ByteString
    , loginVerifier :: ByteString
    , loginRun :: Text
    }

data Session = Session
    { sessionExpires :: UTCTime
    , sessionToken :: OAuthToken
    , sessionCsrf :: ByteString
    }

data Store = Store
    { states :: Map ByteString LoginState
    , sessions :: Map ByteString Session
    }

-- | Create one bounded, process-local store for the lifetime of the daemon.
-- A restart invalidates sessions and pending sign-ins.
browserApplication
    :: BrowserSettings -> BrowserDependencies -> IO Application
browserApplication cfg deps = do
    store <- newMVar $ Store Map.empty Map.empty
    pure $ \req respond -> do
        result <- try @SomeException $ route store req
        respond
            $ fromRight
                ( message
                    status502
                    "Sign-in or report service unavailable. Please try again."
                )
                result
  where
    route store req = do
        now <- browserNow deps
        modifyMVar_ store $ pure . prune now
        case (requestMethod req, pathInfo req) of
            ("GET", ["browser", "report", runId])
                | validRun runId -> do
                    session <- findSession store req
                    maybe (startLogin store now runId) (openReport runId) session
                | otherwise -> pure $ message status400 "Invalid run ID."
            ("GET", ["browser", "callback"]) -> completeLogin store now req
            ("GET", ["browser", "account"]) -> account store req
            ("POST", ["browser", "sign-out"]) -> signOut store req
            (_, ["browser", "sign-out"]) ->
                pure $ message status405 "Use the sign-out form on the account page."
            _ -> pure $ message status404 "Browser page not found."

    startLogin store now runId = do
        state <- browserRandom deps
        binding <- browserRandom deps
        verifier <- browserRandom deps
        inserted <- modifyMVar store $ \s ->
            if Map.size (states s) >= storeLimit
                then pure (s, False)
                else
                    pure
                        ( s
                            { states =
                                Map.insert
                                    state
                                    ( LoginState
                                        (addUTCTime (fromIntegral $ browserStateSeconds cfg) now)
                                        binding
                                        verifier
                                        runId
                                    )
                                    (states s)
                            }
                        , True
                        )
        pure
            $ if inserted
                then
                    redirect
                        (authorize state verifier)
                        [ (hSetCookie, setCookie loginCookie binding (browserStateSeconds cfg))
                        ]
                else
                    message status503 "Sign-in capacity reached. Please try again later."

    authorize state verifier =
        "https://github.com/login/oauth/authorize"
            <> renderQuery
                True
                [ ("client_id", Just $ browserClientId cfg)
                , ("redirect_uri", Just $ browserCallback cfg)
                , ("scope", Just "read:org")
                , ("state", Just state)
                , ("code_challenge_method", Just "S256")
                ,
                    ( "code_challenge"
                    , Just
                        $ Base64.encodeUnpadded (convert (hash verifier :: Digest SHA256))
                    )
                ]

    completeLogin store now req = case (uniqueQuery "state" req, requestCookie loginCookie req) of
        (Just state, Just binding) -> do
            pending <- modifyMVar store $ \s -> case Map.lookup state (states s) of
                Just login
                    | loginBinding login == binding ->
                        pure (s{states = Map.delete state (states s)}, Just login)
                _ -> pure (s, Nothing)
            case pending of
                Just login | loginExpires login > now -> case (uniqueQuery "code" req, lookup "error" $ queryString req) of
                    (Just code, Nothing)
                        | not (BS.null code)
                        , BS.length code <= 2048 -> do
                            token <- browserExchange deps cfg code (loginVerifier login)
                            case token of
                                Nothing ->
                                    pure
                                        $ message
                                            status502
                                            "GitHub sign-in failed. Please open the report again."
                                Just credential -> do
                                    sessionId <- browserRandom deps
                                    csrf <- browserRandom deps
                                    inserted <- modifyMVar store $ \s -> do
                                        let remaining =
                                                maybe
                                                    (sessions s)
                                                    (`Map.delete` sessions s)
                                                    (requestCookie sessionCookie req)
                                        if Map.size remaining >= storeLimit
                                            then pure (s, False)
                                            else
                                                pure
                                                    ( s
                                                        { sessions =
                                                            Map.insert
                                                                sessionId
                                                                ( Session
                                                                    (addUTCTime (fromIntegral $ browserSessionSeconds cfg) now)
                                                                    credential
                                                                    csrf
                                                                )
                                                                remaining
                                                        }
                                                    , True
                                                    )
                                    pure
                                        $ if inserted
                                            then
                                                redirect
                                                    ("/browser/report/" <> T.encodeUtf8 (loginRun login))
                                                    [
                                                        ( hSetCookie
                                                        , setCookie sessionCookie sessionId (browserSessionSeconds cfg)
                                                        )
                                                    , (hSetCookie, clearCookie loginCookie)
                                                    ]
                                            else
                                                message status503 "Session capacity reached. Please try again later."
                    _ ->
                        pure
                            $ message
                                status400
                                "GitHub sign-in was cancelled or invalid. Please open the report again."
                _ -> pure invalidCallback
        _ -> pure invalidCallback

    findSession store req = modifyMVar store $ \s ->
        pure
            (s, requestCookie sessionCookie req >>= (`Map.lookup` sessions s))

    openReport runId session = do
        identity <- browserIdentify deps (sessionToken session)
        case identity of
            Left GitHubError{status = 401} ->
                pure
                    $ message
                        status401
                        "GitHub sign-in expired. Sign out and open the report again."
            Left _ ->
                pure
                    $ message
                        status502
                        "GitHub identity service unavailable. Please try again."
            Right login ->
                browserMembership deps (sessionToken session) login >>= \case
                    Active ->
                        browserReport deps runId >>= \case
                            ReportReady url -> pure $ redirect url []
                            ReportPending ->
                                pure
                                    $ message
                                        status409
                                        "The test is still starting or running. Please try again when it finishes."
                            ReportMissing -> pure $ message status404 "No report is available for this run."
                            ReportUnavailable ->
                                pure
                                    $ message status502 "Report service unavailable. Please try again."
                    Pending -> pure forbidden
                    NotMember -> pure forbidden
                    TokenInvalid ->
                        pure
                            $ message
                                status401
                                "GitHub sign-in expired. Sign out and open the report again."
                    _ ->
                        pure
                            $ message
                                status502
                                "GitHub membership could not be verified. Please try again."

    account store req =
        findSession store req >>= \case
            Nothing ->
                pure
                    $ message
                        status401
                        "You are signed out. Open a report to sign in with GitHub."
            Just session ->
                pure
                    $ privateResponse status200 [(hContentType, "text/html; charset=utf-8")]
                    $ LBS.fromStrict
                    $ "<!doctype html><html><head><title>Antithesis proxy account</title></head><body><h1>Antithesis proxy account</h1><p>You are signed in with GitHub. Active team membership is checked each time you open a report.</p><form method=\"post\" action=\"/browser/sign-out\"><input type=\"hidden\" name=\"csrf\" value=\""
                        <> sessionCsrf session
                        <> "\"><button type=\"submit\">Sign out</button></form></body></html>"

    signOut store req = do
        session <- findSession store req
        if uniqueHeader hOrigin req /= Just (callbackOrigin cfg)
            then
                pure
                    $ message status403 "Sign-out must use the account form on this origin."
            else case session of
                Nothing ->
                    pure
                        $ redirect "/browser/account" [(hSetCookie, clearCookie sessionCookie)]
                Just signedIn -> do
                    -- The only accepted body is the small generated form. Avoid
                    -- reading arbitrary attacker-controlled request bodies.
                    case uniqueHeader hContentLength req >>= readLength of
                        Just len | len <= 256 -> do
                            body <- strictRequestBody req
                            if uniqueValue "csrf" (parseQuery $ LBS.toStrict body)
                                == Just (sessionCsrf signedIn)
                                then do
                                    modifyMVar_ store $ \s ->
                                        pure
                                            s
                                                { sessions =
                                                    maybe
                                                        (sessions s)
                                                        (`Map.delete` sessions s)
                                                        (requestCookie sessionCookie req)
                                                }
                                    pure
                                        $ redirect "/browser/account" [(hSetCookie, clearCookie sessionCookie)]
                                else pure $ message status403 "Invalid sign-out form."
                        _ -> pure $ message status403 "Invalid sign-out form."

-- | Wire OAuth, fresh GitHub checks and Antithesis report lookup. Endpoints
-- are constructor arguments for HTTP tests, never caller-supplied URLs.
browserDependencies
    :: HC.Manager
    -> String
    -> AuthConfig
    -> UpstreamConfig
    -> String
    -> IO BrowserDependencies
browserDependencies manager tokenEndpoint auth upstream tenant =
    pure
        BrowserDependencies
            { browserNow = authNow auth
            , browserRandom =
                Base64.encodeUnpadded <$> (getRandomBytes 32 :: IO ByteString)
            , browserExchange = exchange
            , browserIdentify = authWhoami auth
            , browserMembership = \token login ->
                authCheckTeamMembership
                    auth
                    token
                    (authOrg auth)
                    (authTeam auth)
                    login
            , browserReport = fetchReport
            }
  where
    exchange cfg code verifier = do
        req <- HC.parseRequest tokenEndpoint
        response <-
            HC.httpLbs
                ( HC.urlEncodedBody
                    [ ("client_id", browserClientId cfg)
                    , ("client_secret", browserClientSecret cfg)
                    , ("redirect_uri", browserCallback cfg)
                    , ("code", code)
                    , ("code_verifier", verifier)
                    ]
                    req
                        { HC.requestHeaders = [(hAccept, "application/json")]
                        , HC.redirectCount = 0
                        , HC.responseTimeout = HC.responseTimeoutMicro 10000000
                        }
                )
                manager
        pure
            $ if HC.responseStatus response == status200
                then case eitherDecode (HC.responseBody response) of
                    Right (TokenResponse token tokenType)
                        | not (T.null token)
                        , T.toLower tokenType == "bearer" ->
                            Just $ OAuthToken $ T.encodeUtf8 token
                    _ -> Nothing
                else Nothing

    fetchReport runId = do
        req <-
            HC.parseRequest
                $ showBaseUrl (upstreamBaseUrl upstream)
                    <> "/api/v0/runs/"
                    <> T.unpack runId
        response <-
            HC.httpLbs
                req
                    { HC.requestHeaders =
                        [(hAuthorization, "Bearer " <> upstreamApiKey upstream)]
                    , HC.redirectCount = 0
                    , HC.responseTimeout = HC.responseTimeoutMicro 10000000
                    }
                (upstreamManager upstream)
        case statusCode $ HC.responseStatus response of
            404 -> pure ReportMissing
            200 -> case eitherDecode (HC.responseBody response) of
                Right (RunReport runState report)
                    | runState
                        `elem` ["starting", "running", "in_progress", "pending", "queued"] ->
                        pure ReportPending
                    | Just url <- report ->
                        pure
                            $ if safeDestination tenant url
                                then ReportReady (T.encodeUtf8 url)
                                else ReportUnavailable
                    | otherwise -> pure ReportMissing
                Left _ -> pure ReportUnavailable
            _ -> pure ReportUnavailable

data TokenResponse = TokenResponse Text Text

instance FromJSON TokenResponse where
    parseJSON = withObject "OAuth response" $ \o -> TokenResponse <$> o .: "access_token" <*> o .: "token_type"

data RunReport = RunReport Text (Maybe Text)
instance FromJSON RunReport where
    parseJSON = withObject "Run" $ \o -> do
        runState <- o .:? "status" .!= ""
        links <- o .:? "links"
        report <-
            maybe
                (pure Nothing)
                (withObject "Run links" (.:? "triage_report"))
                links
        pure $ RunReport runState report

prune :: UTCTime -> Store -> Store
prune now s =
    Store
        (Map.filter ((> now) . loginExpires) (states s))
        (Map.filter ((> now) . sessionExpires) (sessions s))

validRun :: Text -> Bool
validRun runId = not (T.null runId) && T.length runId <= 128 && T.all allowed runId
  where
    allowed c =
        isAsciiLower c
            || isAsciiUpper c
            || isDigit c
            || c `elem` ['-', '_']

safeDestination :: String -> Text -> Bool
safeDestination tenant url =
    "https://" `T.isPrefixOf` base
        && not (T.null host)
        && T.all validHost host
        && (base <> "/report/") `T.isPrefixOf` url
        && not
            (T.any (\c -> c <= ' ' || c >= '\DEL' || c == '#' || c == '\\') url)
        && length segments >= 3
        && all validSegment segments
  where
    base = T.dropWhileEnd (== '/') $ T.pack tenant
    host = T.drop 8 base
    validHost c =
        isAsciiLower c
            || isDigit c
            || c `elem` ['.', '-']
    path = T.takeWhile (/= '?') $ T.drop (T.length base + 1) url
    segments = T.splitOn "/" path
    validSegment s =
        not (T.null s)
            && s /= "."
            && s /= ".."
            && not (T.any (\c -> c == '%' || c == ':') s)

callbackOrigin :: BrowserSettings -> ByteString
callbackOrigin cfg =
    BS.take (BS.length callback - BS.length "/browser/callback") callback
  where
    callback = browserCallback cfg

uniqueQuery :: ByteString -> Request -> Maybe ByteString
uniqueQuery name = uniqueValue name . queryString

uniqueValue :: ByteString -> Query -> Maybe ByteString
uniqueValue name values = case [v | (k, Just v) <- values, k == name] of
    [value] | length (filter ((== name) . fst) values) == 1 -> Just value
    _ -> Nothing

uniqueHeader :: HeaderName -> Request -> Maybe ByteString
uniqueHeader name req = case [v | (k, v) <- requestHeaders req, k == name] of
    [value] -> Just value
    _ -> Nothing

requestCookie :: ByteString -> Request -> Maybe ByteString
requestCookie name req = do
    raw <- uniqueHeader hCookie req
    let cookies =
            [ (BC.dropWhile (== ' ') k, Just $ BS.drop 1 v)
            | item <- BC.split ';' raw
            , let (k, v) = BC.break (== '=') item
            ]
    value <- uniqueValue name cookies
    guard
        $ BS.length value == 43
            && BC.all
                (\c -> c `elem` ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "-_")
                value
    pure value

setCookie :: ByteString -> ByteString -> Int -> ByteString
setCookie name value seconds =
    name
        <> "="
        <> value
        <> "; Path=/; Max-Age="
        <> BC.pack (show seconds)
        <> "; Secure; HttpOnly; SameSite=Lax"

clearCookie :: ByteString -> ByteString
clearCookie name = setCookie name "" 0

sessionCookie, loginCookie :: ByteString
sessionCookie = "__Host-moog-session"
loginCookie = "__Host-moog-login"

storeLimit :: Int
storeLimit = 10000

readLength :: ByteString -> Maybe Int
readLength bytes = case BC.readInt bytes of
    Just (n, rest) | n >= 0 && BS.null rest -> Just n
    _ -> Nothing

invalidCallback, forbidden :: Response
invalidCallback =
    message
        status400
        "Invalid or expired sign-in. Please open the report again."
forbidden =
    message
        status403
        "Report access requires active membership of the authorized GitHub team."

message :: Status -> LBS.ByteString -> Response
message st = privateResponse st [(hContentType, "text/plain; charset=utf-8")]

redirect :: ByteString -> ResponseHeaders -> Response
redirect destination headers = privateResponse status303 ((hLocation, destination) : headers) ""

-- | Headers on every browser response, including errors and redirects.
privateResponse
    :: Status -> ResponseHeaders -> LBS.ByteString -> Response
privateResponse st headers =
    responseLBS
        st
        ( [ (hCacheControl, "no-store")
          , ("Referrer-Policy", "no-referrer")
          , ("X-Content-Type-Options", "nosniff")
          ,
              ( "Content-Security-Policy"
              , "default-src 'none'; form-action 'self'; frame-ancestors 'none'; base-uri 'none'"
              )
          ]
            <> headers
        )
