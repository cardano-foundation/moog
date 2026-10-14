{-# LANGUAGE OverloadedStrings #-}

module Proxy.Antithesis.BrowserSpec (spec) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, throwIO)
import Control.Monad (forM_, join)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (encode, object, (.=))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as Base64
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Maybe (fromJust, listToMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Lib.GitHub.Auth.Identify (Login (..))
import Lib.GitHub.Auth.Identify.Internal
    ( IdentifyEndpoint (..)
    , whoamiWith
    )
import Lib.GitHub.Auth.TeamCheck (Org (..), TeamSlug (..))
import Lib.GitHub.Auth.TeamCheck.Internal
    ( TeamCheckEndpoint (..)
    , checkTeamMembershipWith
    )
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types
    ( HeaderName
    , Query
    , RequestHeaders
    , Status
    , hAuthorization
    , hCacheControl
    , hContentType
    , hCookie
    , hLocation
    , parseQuery
    , status200
    , status401
    , status403
    , status404
    , status500
    , statusCode
    )
import Network.HTTP.Types.Header (hOrigin, hSetCookie)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Proxy.Antithesis.Application
    ( ProxyApplicationConfig (..)
    , ReadinessConfig (..)
    , proxyApplication
    )
import Proxy.Antithesis.Browser
    ( browserApplication
    , browserDependencies
    )
import Proxy.Antithesis.Cache (newMembershipCache)
import Proxy.Antithesis.Config (BrowserSettings (..))
import Proxy.Antithesis.Middleware.Auth (AuthConfig (..))
import Proxy.Antithesis.Server (UpstreamConfig (..), makeServantApp)
import Servant.Client (parseBaseUrl)
import System.IO
    ( SeekMode (AbsoluteSeek)
    , hClose
    , hFlush
    , hSeek
    , stdout
    )
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldNotBe
    , shouldReturn
    , shouldSatisfy
    )

spec :: Spec
spec = describe "Proxy.Antithesis.Browser HTTP" $ do
    it
        "uses browser-bound state and S256 PKCE and resumes only the selected run"
        $ withFixture
        $ \f -> do
            (state, binding, authorize) <-
                begin f "run-one?return_to=https://evil.example"
            queryValue "code_challenge_method" authorize `shouldBe` Just "S256"
            signedIn <- callback f state binding "good"
            status signedIn `shouldBe` 303
            header hLocation signedIn `shouldBe` Just "/browser/report/run-one"
            let session = sessionCookie signedIn
            session `shouldNotBe` binding
            assertPrivate signedIn
            header hSetCookie signedIn
                `shouldSatisfy` maybe False (BS.isInfixOf "Secure; HttpOnly; SameSite=Lax")
            exchanges <- readIORef (fExchanges f)
            length exchanges `shouldBe` 1
            let exchange = fromJust $ listToMaybe exchanges
                verifier = fromJust $ join $ lookup "code_verifier" exchange
                challenge = Base64.encodeUnpadded (convert (hash verifier :: Digest SHA256))
            queryValue "code_challenge" authorize `shouldBe` Just challenge
            lookup "client_secret" exchange
                `shouldBe` Just (Just "mock-client-secret")
            opened <- get f "/browser/report/run-one" session
            status opened `shouldBe` 303
            header hLocation opened `shouldBe` Just reportUrl
            assertPrivate opened
            readIORef (fReportCalls f) `shouldReturn` 1
            readIORef (fMembershipCalls f) `shouldReturn` 1

    it "refuses missing and mismatched browser binding before exchange" $ withFixture $ \f -> do
        (state, binding, _) <- begin f "run-one"
        let bindingValue = BS.drop 1 $ snd $ BS.break (== '=') binding
            wrongValue = BS.map (\c -> if c == 'A' then 'B' else 'A') bindingValue
        forM_ ["", "__Host-moog-login=" <> wrongValue] $ \cookie -> do
            refused <- callback f state cookie "good"
            status refused `shouldBe` 400
        readIORef (fExchanges f) `shouldReturn` []
        valid <- callback f state binding "good"
        status valid `shouldBe` 303

    it
        "refuses unknown state, missing code and duplicated binding cookies before exchange"
        $ withFixture
        $ \f -> do
            (state, binding, _) <- begin f "run-one"
            unknown <- callback f (state <> "x") binding "good"
            status unknown `shouldBe` 400
            duplicated <- callback f state (binding <> "; " <> binding) "good"
            status duplicated `shouldBe` 400
            missingCode <- get f ("/browser/callback?state=" <> state) binding
            status missingCode `shouldBe` 400
            readIORef (fExchanges f) `shouldReturn` []

    it "consumes callbacks once and refuses replay before exchange" $ withFixture $ \f -> do
        (state, binding, _) <- begin f "run-one"
        first <- callback f state binding "good"
        status first `shouldBe` 303
        replay <- callback f state binding "good"
        status replay `shouldBe` 400
        length <$> readIORef (fExchanges f) `shouldReturn` 1

    it "allows only one exchange when callbacks arrive concurrently" $ withFixture $ \f -> do
        (state, binding, _) <- begin f "run-one"
        completed <- newEmptyMVar
        _ <- forkFinally (callback f state binding "good") (putMVar completed)
        response <- callback f state binding "good"
        other <- takeMVar completed >>= either throwIO pure
        [status response, status other]
            `shouldSatisfy` (\codes -> 303 `elem` codes && 400 `elem` codes)
        length <$> readIORef (fExchanges f) `shouldReturn` 1

    it
        "emits no browser request logs for sign-in, allowed redirects or denial"
        $ withFixture
        $ \f -> do
            captured <- withSystemTempFile "browser-request-log" $ \_ handle -> do
                hFlush stdout
                bracket
                    (hDuplicate stdout)
                    (\saved -> hFlush stdout >> hDuplicateTo saved stdout >> hClose saved)
                    $ \_ -> do
                        hDuplicateTo handle stdout
                        cookie <- login f
                        allowed <- get f "/browser/report/run-one" cookie
                        status allowed `shouldBe` 303
                        writeIORef (fMembership f) (status404, "absent")
                        denied <- get f "/browser/report/run-one" cookie
                        status denied `shouldBe` 403
                        hFlush stdout
                        hSeek handle AbsoluteSeek 0
                        BS.hGetContents handle
            captured `shouldBe` ""

    it "refuses expired login state before exchange" $ withFixture $ \f -> do
        (state, binding, _) <- begin f "run-one"
        modifyIORef' (fNow f) (addUTCTime 300)
        expired <- callback f state binding "good"
        status expired `shouldBe` 400
        readIORef (fExchanges f) `shouldReturn` []

    it "cancels login without granting a session or reusing state" $ withFixture $ \f -> do
        (state, binding, _) <- begin f "run-one"
        cancelled <-
            get
                f
                ("/browser/callback?state=" <> state <> "&error=access_denied")
                binding
        status cancelled `shouldBe` 400
        replay <- callback f state binding "good"
        status replay `shouldBe` 400
        readIORef (fExchanges f) `shouldReturn` []

    it
        "fails safely on token exchange and identity errors without reflecting private bodies"
        $ withFixture
        $ \f -> do
            (state, binding, _) <- begin f "run-one"
            writeIORef (fTokenStatus f) status500
            failed <- callback f state binding "good"
            status failed `shouldBe` 502
            LBS.toStrict (HC.responseBody failed)
                `shouldSatisfy` (not . BS.isInfixOf "mock-private")
            writeIORef (fTokenStatus f) status200
            cookie <- login f
            writeIORef (fIdentityStatus f) status401
            denied <- get f "/browser/report/run-one" cookie
            status denied `shouldBe` 401
            readIORef (fReportCalls f) `shouldReturn` 0

    it
        "checks identity and membership afresh for every disclosure and observes removal"
        $ withFixture
        $ \f -> do
            cookie <- login f
            allowed <- get f "/browser/report/run-one" cookie
            status allowed `shouldBe` 303
            writeIORef (fMembership f) (status404, "not a member")
            removed <- get f "/browser/report/run-two" cookie
            status removed `shouldBe` 403
            header hLocation removed `shouldBe` Nothing
            readIORef (fReportCalls f) `shouldReturn` 1
            readIORef (fMembershipCalls f) `shouldReturn` 2
            readIORef (fIdentityCalls f) `shouldReturn` 2

    forM_
        [ (status200, "pending", 403)
        , (status404, "absent", 403)
        , (status403, "SSO", 502)
        , (status500, "mock-private", 502)
        ]
        $ \(upstreamStatus, memberState, expected) ->
            it
                ( "denies direct resolver requests for membership "
                    <> BS.unpack memberState
                )
                $ withFixture
                $ \f -> do
                    cookie <- login f
                    writeIORef (fMembership f) (upstreamStatus, memberState)
                    denied <- get f "/browser/report/run-one" cookie
                    status denied `shouldBe` expected
                    header hLocation denied `shouldBe` Nothing
                    assertPrivate denied
                    readIORef (fReportCalls f) `shouldReturn` 0

    it "starts new sign-in at exact session expiry" $ withFixture $ \f -> do
        cookie <- login f
        modifyIORef' (fNow f) (addUTCTime 3600)
        expired <- get f "/browser/report/run-one" cookie
        status expired `shouldBe` 303
        header hLocation expired
            `shouldSatisfy` maybe
                False
                (BS.isPrefixOf "https://github.com/login/oauth/authorize?")
        readIORef (fReportCalls f) `shouldReturn` 0

    it
        "protects sign-out with origin and CSRF then invalidates the session"
        $ withFixture
        $ \f -> do
            cookie <- login f
            account <- get f "/browser/account" cookie
            let body = LBS.toStrict $ HC.responseBody account
                csrf =
                    BS.takeWhile (/= '"')
                        $ BS.drop 7
                        $ snd
                        $ BS.breakSubstring "value=\"" body
            BS.null csrf `shouldBe` False
            forM_ [[], [(hOrigin, "https://evil.example")]] $ \headers -> do
                refused <-
                    send
                        f
                        "POST"
                        "/browser/sign-out"
                        ((hCookie, cookie) : headers)
                        ("csrf=" <> csrf)
                status refused `shouldBe` 403
            wrong <-
                send
                    f
                    "POST"
                    "/browser/sign-out"
                    [(hCookie, cookie), (hOrigin, origin)]
                    "csrf=wrong"
            status wrong `shouldBe` 403
            signedOut <-
                send
                    f
                    "POST"
                    "/browser/sign-out"
                    [(hCookie, cookie), (hOrigin, origin)]
                    ("csrf=" <> csrf)
            status signedOut `shouldBe` 303
            again <- get f "/browser/report/run-one" cookie
            status again `shouldBe` 303
            header hLocation again
                `shouldSatisfy` maybe False (BS.isPrefixOf "https://github.com/")
            readIORef (fReportCalls f) `shouldReturn` 0

    it
        "never authorizes CLI routes with cookies and preserves bearer access"
        $ withFixture
        $ \f -> do
            cookie <- login f
            denied <- get f "/api/v0/runs" cookie
            status denied `shouldBe` 401
            allowed <-
                send
                    f
                    "GET"
                    "/api/v0/runs"
                    [(hAuthorization, "Bearer mock-oauth-token")]
                    ""
            status allowed `shouldBe` 200

    it "distinguishes running, missing and upstream report failures" $ withFixture $ \f -> do
        cookie <- login f
        forM_
            [ (status200, "running", 409)
            , (status200, "missing", 404)
            , (status404, "missing", 404)
            , (status500, "mock-private", 502)
            ]
            $ \(upstreamStatus, runState, expected) -> do
                writeIORef (fRun f) (upstreamStatus, runState, Nothing)
                result <- get f "/browser/report/run-one" cookie
                status result `shouldBe` expected
                header hLocation result `shouldBe` Nothing

    it "refuses unsafe destinations even when supplied by Antithesis" $ withFixture $ \f -> do
        cookie <- login f
        forM_
            [ "http://tenant.antithesis.com/report/id/key.html"
            , "https://evil.example/report/id/key.html"
            , "https://tenant.antithesis.com.evil.example/report/id/key.html"
            , "https://tenant.antithesis.com/api/v0/runs"
            , "https://tenant.antithesis.com:8443/report/id/key.html"
            , "https://user@tenant.antithesis.com/report/id/key.html"
            , "https://tenant.antithesis.com/report/../api/key.html"
            , "https://tenant.antithesis.com/report/%2e%2e/api/key.html"
            , "https://tenant.antithesis.com/report/id/key.html#fragment"
            ]
            $ \url -> do
                writeIORef (fRun f) (status200, "completed", Just url)
                result <- get f "/browser/report/run-one" cookie
                status result `shouldBe` 502
                header hLocation result `shouldBe` Nothing

    it "validates run IDs and refuses duplicate callback arguments" $ withFixture $ \f -> do
        forM_
            [ "/browser/report/.."
            , "/browser/report/a%2Fb"
            , "/browser/report/a%0Ab"
            ]
            $ \path -> do
                result <- get f path ""
                status result `shouldBe` 400
        (state, binding, _) <- begin f "run-one"
        duplicate <-
            get
                f
                ( "/browser/callback?state="
                    <> state
                    <> "&state="
                    <> state
                    <> "&code=good"
                )
                binding
        status duplicate `shouldBe` 400
        readIORef (fExchanges f) `shouldReturn` []

    it "rotates sign-in sessions and denies the old identifier" $ withFixture $ \f -> do
        old <- login f
        start <- get f "/browser/report/run-two" ""
        let state = fromJust $ header hLocation start >>= queryValue "state"
            binding = bindingCookie start
        newer <- callback f state (binding <> "; " <> old) "good"
        sessionCookie newer `shouldNotBe` old
        stale <- get f "/browser/report/run-one" old
        header hLocation stale
            `shouldSatisfy` maybe False (BS.isPrefixOf "https://github.com/")

-- The browser and both upstream boundaries run on real loopback HTTP sockets.
data Fixture = Fixture
    { fBase :: String
    , fManager :: HC.Manager
    , fNow :: IORef UTCTime
    , fMembership :: IORef (Status, ByteString)
    , fRun :: IORef (Status, Text, Maybe ByteString)
    , fTokenStatus :: IORef Status
    , fIdentityStatus :: IORef Status
    , fExchanges :: IORef [Query]
    , fReportCalls :: IORef Int
    , fMembershipCalls :: IORef Int
    , fIdentityCalls :: IORef Int
    }

withFixture :: (Fixture -> IO a) -> IO a
withFixture action = do
    manager <- HC.newManager HC.defaultManagerSettings
    now <- newIORef $ UTCTime (toEnum 0) 0
    membership <- newIORef (status200, "active")
    run <- newIORef (status200, "completed", Just reportUrl)
    tokenStatus <- newIORef status200
    identityStatus <- newIORef status200
    exchanges <- newIORef []
    reportCalls <- newIORef 0
    membershipCalls <- newIORef 0
    identityCalls <- newIORef 0
    let fixture =
            Fixture
                ""
                manager
                now
                membership
                run
                tokenStatus
                identityStatus
                exchanges
                reportCalls
                membershipCalls
                identityCalls
    testWithApplication (pure $ mockUpstream fixture) $ \upstreamPort -> do
        let upstream = "http://127.0.0.1:" <> show upstreamPort
        parsed <- parseBaseUrl upstream
        cache <- newMembershipCache 60
        let auth =
                AuthConfig
                    (Org "pragma-org")
                    (TeamSlug "antithesis-access")
                    cache
                    (readIORef now)
                    (whoamiWith $ IdentifyEndpoint upstream)
                    ( \token org team (Login username) ->
                        checkTeamMembershipWith
                            (TeamCheckEndpoint upstream)
                            token
                            org
                            team
                            username
                    )
            cfg =
                BrowserSettings
                    "mock-client-id"
                    "mock-client-secret"
                    (origin <> "/browser/callback")
                    3600
                    300
        deps <-
            browserDependencies
                manager
                (upstream <> "/login/oauth/access_token")
                auth
                (UpstreamConfig parsed "mock-api-key" manager)
                "https://tenant.antithesis.com"
        browser <- browserApplication cfg deps
        let app =
                proxyApplication
                    $ ProxyApplicationConfig
                        auth
                        (makeServantApp $ UpstreamConfig parsed "mock-api-key" manager)
                        (ReadinessConfig (pure True) (pure True))
                        (Just browser)
        testWithApplication (pure app) $ \port -> action fixture{fBase = "http://127.0.0.1:" <> show port}

mockUpstream :: Fixture -> Wai.Application
mockUpstream f req respond = case Wai.pathInfo req of
    ["login", "oauth", "access_token"] -> do
        body <- Wai.strictRequestBody req
        modifyIORef' (fExchanges f) (<> [parseQuery $ LBS.toStrict body])
        st <- readIORef (fTokenStatus f)
        respond
            $ Wai.responseLBS
                st
                [(hContentType, "application/json")]
                "{\"access_token\":\"mock-oauth-token\",\"token_type\":\"bearer\"}"
    ["user"] -> do
        modifyIORef' (fIdentityCalls f) (+ 1)
        st <- readIORef (fIdentityStatus f)
        respond
            $ Wai.responseLBS
                st
                [(hContentType, "application/json")]
                "{\"login\":\"octocat\"}"
    [ "orgs"
        , "pragma-org"
        , "teams"
        , "antithesis-access"
        , "memberships"
        , "octocat"
        ] -> do
            modifyIORef' (fMembershipCalls f) (+ 1)
            (st, memberState) <- readIORef (fMembership f)
            respond
                $ Wai.responseLBS
                    st
                    [(hContentType, "application/json")]
                    (encode $ object ["state" .= BS.unpack memberState])
    ["api", "v0", "runs", _] -> do
        lookup hAuthorization (Wai.requestHeaders req)
            `shouldBe` Just "Bearer mock-api-key"
        modifyIORef' (fReportCalls f) (+ 1)
        (st, runState, report) <- readIORef (fRun f)
        respond
            $ Wai.responseLBS
                st
                [(hContentType, "application/json")]
                ( encode
                    $ object
                        [ "status" .= runState
                        , "links" .= object ["triage_report" .= fmap BS.unpack report]
                        ]
                )
    ["api", "v0", "runs"] ->
        respond
            $ Wai.responseLBS status200 [(hContentType, "application/json")] "[]"
    _ -> respond $ Wai.responseLBS status404 [] "missing"

send
    :: Fixture
    -> ByteString
    -> ByteString
    -> RequestHeaders
    -> ByteString
    -> IO (HC.Response LBS.ByteString)
send f method path headers body = do
    req <- HC.parseRequest (fBase f <> BS.unpack path)
    HC.httpLbs
        req
            { HC.method = method
            , HC.requestHeaders = headers
            , HC.requestBody = HC.RequestBodyBS body
            , HC.redirectCount = 0
            , HC.checkResponse = \_ _ -> pure ()
            }
        (fManager f)

get
    :: Fixture -> ByteString -> ByteString -> IO (HC.Response LBS.ByteString)
get f path cookie = send f "GET" path [(hCookie, cookie)] ""

begin
    :: Fixture -> ByteString -> IO (ByteString, ByteString, ByteString)
begin f runId = do
    response <- get f ("/browser/report/" <> runId) ""
    status response `shouldBe` 303
    let authorize = fromJust $ header hLocation response
    pure
        ( fromJust $ queryValue "state" authorize
        , bindingCookie response
        , authorize
        )

callback
    :: Fixture
    -> ByteString
    -> ByteString
    -> ByteString
    -> IO (HC.Response LBS.ByteString)
callback f state cookie code =
    get f ("/browser/callback?state=" <> state <> "&code=" <> code) cookie

login :: Fixture -> IO ByteString
login f = do
    (state, binding, _) <- begin f "run-one"
    response <- callback f state binding "good"
    status response `shouldBe` 303
    pure $ sessionCookie response

status :: HC.Response a -> Int
status = statusCode . HC.responseStatus

header :: HeaderName -> HC.Response a -> Maybe ByteString
header name = lookup name . HC.responseHeaders

queryValue :: ByteString -> ByteString -> Maybe ByteString
queryValue name uri = join $ lookup name (parseQuery $ snd $ BS.break (== '?') uri)

bindingCookie :: HC.Response a -> ByteString
bindingCookie = cookieStarting "__Host-moog-login="

sessionCookie :: HC.Response a -> ByteString
sessionCookie = cookieStarting "__Host-moog-session="

cookieStarting :: ByteString -> HC.Response a -> ByteString
cookieStarting prefix response =
    fromJust
        $ listToMaybe
            [ BS.takeWhile (/= ';') value
            | (key, value) <- HC.responseHeaders response
            , key == hSetCookie
            , prefix `BS.isPrefixOf` value
            ]

assertPrivate :: HC.Response a -> IO ()
assertPrivate response = do
    header hCacheControl response `shouldBe` Just "no-store"
    header "Referrer-Policy" response `shouldBe` Just "no-referrer"

origin :: ByteString
origin = "https://proxy.example"

reportUrl :: ByteString
reportUrl =
    "https://tenant.antithesis.com/report/run/key.html?signature=mock-private"
