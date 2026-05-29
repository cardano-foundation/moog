{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | WAI application assembly for the Antithesis proxy daemon.
--
-- The proxy is a Servant 'AntithesisProxyAPI' server wrapped in a
-- GitHub-team-gate WAI middleware. Two trivial routes live outside the
-- Servant subtree: @/healthz@ and @/readyz@.
module Proxy.Antithesis.Application
    ( ProxyApplicationConfig (..)
    , ReadinessConfig (..)
    , productionApplication
    , proxyApplication
    , readinessCheck
    )
where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Vault.Lazy qualified as Vault
import Lib.GitHub.Auth.Identify (whoami)
import Lib.GitHub.Auth.TeamCheck (checkTeamMembership)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types
    ( RequestHeaders
    , Status
    , hAccept
    , hAuthorization
    , hContentType
    , hUserAgent
    , methodGet
    , status200
    , status503
    , statusCode
    )
import Network.Wai
    ( Application
    , Request (pathInfo, rawPathInfo, requestHeaders, requestMethod, vault)
    , Response
    , responseLBS
    )
import Network.Wai.Internal (Response (..))
import Proxy.Antithesis.Audit
    ( AuditEvent (..)
    , RequestId (..)
    , renderAuditEvent
    )
import Proxy.Antithesis.Cache (newMembershipCache)
import Proxy.Antithesis.Config
    ( Settings (..)
    )
import Proxy.Antithesis.Middleware.Auth
    ( AuthConfig (..)
    , authMiddleware
    , authorizedLoginKey
    )
import Proxy.Antithesis.Server
    ( UpstreamConfig (..)
    , makeServantApp
    )
import Servant.Client (parseBaseUrl)
import System.IO (hFlush, stdout)

data ProxyApplicationConfig = ProxyApplicationConfig
    { proxyAuthConfig :: AuthConfig
    , proxyServantApp :: Application
    , proxyReadinessConfig :: ReadinessConfig
    }

data ReadinessConfig = ReadinessConfig
    { readinessAntithesis :: IO Bool
    , readinessGitHub :: IO Bool
    }

proxyApplication :: ProxyApplicationConfig -> Application
proxyApplication config request respond =
    case (requestMethod request, pathInfo request) of
        ("GET", ["healthz"]) -> do
            start <- getCurrentTime
            auditAndRespond request start Nothing (plain status200 "ok") respond
        ("GET", ["readyz"]) -> do
            start <- getCurrentTime
            ready <- readinessCheck $ proxyReadinessConfig config
            auditAndRespond
                request
                start
                Nothing
                ( if ready
                    then plain status200 "ready"
                    else plain status503 "not ready"
                )
                respond
        _ -> protectedRoute (proxyAuthConfig config) (proxyServantApp config) request respond

-- | Run the auth middleware against the request; on success pass the
-- (auth-attached) request to @innerApp@, audit the response status, and
-- forward to the caller. On failure, audit the rejection and pass it
-- through.
protectedRoute
    :: AuthConfig
    -> Application
    -> Application
protectedRoute authCfg innerApp request respond = do
    authHandled <- newIORef False
    start <- getCurrentTime
    authMiddleware
        authCfg
        ( \authorizedRequest authorizedRespond -> do
            writeIORef authHandled True
            innerApp authorizedRequest $ \response ->
                auditAndRespond
                    authorizedRequest
                    start
                    (Just $ waiResponseStatus response)
                    response
                    authorizedRespond
        )
        request
        ( \response -> do
            handled <- readIORef authHandled
            if handled
                then respond response
                else
                    auditAndRespond
                        request
                        start
                        Nothing
                        response
                        respond
        )

readinessCheck :: ReadinessConfig -> IO Bool
readinessCheck config =
    (&&)
        <$> readinessAntithesis config
        <*> readinessGitHub config

productionApplication :: Settings -> IO Application
productionApplication settings = do
    manager <- HC.newManager tlsManagerSettings
    cache <-
        newMembershipCache $
            fromIntegral (settingsMembershipTtlSeconds settings)
    baseUrl <- parseBaseUrl (settingsAntithesisUrl settings)
    let upstreamCfg =
            UpstreamConfig
                { upstreamBaseUrl = baseUrl
                , upstreamApiKey = settingsAntithesisApiKey settings
                , upstreamManager = manager
                }
        servantApp = makeServantApp upstreamCfg
    pure $
        proxyApplication
            ProxyApplicationConfig
                { proxyAuthConfig =
                    AuthConfig
                        { authOrg = settingsAuthorizedOrg settings
                        , authTeam = settingsAuthorizedTeam settings
                        , authCache = cache
                        , authNow = getCurrentTime
                        , authWhoami = whoami
                        , authCheckTeamMembership = checkTeamMembership
                        }
                , proxyServantApp = servantApp
                , proxyReadinessConfig =
                    productionReadinessConfig settings manager
                }

productionReadinessConfig :: Settings -> HC.Manager -> ReadinessConfig
productionReadinessConfig settings manager =
    ReadinessConfig
        { readinessAntithesis =
            probeEndpoint
                manager
                (antithesisRunsUrl settings)
                [(hAuthorization, antithesisBearerAuthorization settings)]
        , readinessGitHub =
            probeEndpoint
                manager
                "https://api.github.com/rate_limit"
                [ (hAccept, "application/vnd.github+json")
                , (hUserAgent, "moog-antithesis-proxy")
                ]
        }

probeEndpoint :: HC.Manager -> String -> RequestHeaders -> IO Bool
probeEndpoint manager url headers = do
    result <- try @SomeException $ do
        baseRequest <- HC.parseRequest url
        response <-
            HC.httpLbs
                baseRequest
                    { HC.method = methodGet
                    , HC.requestHeaders = headers
                    }
                manager
        pure $ statusCode (HC.responseStatus response) < 500
    pure $ either (const False) id result

antithesisRunsUrl :: Settings -> String
antithesisRunsUrl settings =
    stripTrailingSlash (settingsAntithesisUrl settings) <> "/api/v0/runs"

antithesisBearerAuthorization :: Settings -> ByteString
antithesisBearerAuthorization settings =
    "Bearer " <> settingsAntithesisApiKey settings

plain :: Status -> ByteString -> Response
plain status body =
    responseLBS status [(hContentType, "text/plain")] $ LBS.fromStrict body

stripTrailingSlash :: String -> String
stripTrailingSlash =
    reverse . dropWhile (== '/') . reverse

auditAndRespond
    :: Request
    -> UTCTime
    -> Maybe Status
    -> Response
    -> (Response -> IO responseReceived)
    -> IO responseReceived
auditAndRespond request start upstreamStatus response respond = do
    end <- getCurrentTime
    LBS.putStr $
        renderAuditEvent
            AuditEvent
                { auditTimestamp = end
                , auditLogin =
                    Vault.lookup authorizedLoginKey (vault request)
                , auditPath =
                    T.decodeUtf8With lenientDecode $ rawPathInfo request
                , auditStatus = waiResponseStatus response
                , auditUpstreamStatus = upstreamStatus
                , auditLatencyMs =
                    floor $ diffUTCTime end start * 1000
                , auditRequestId = requestId request
                }
    hFlush stdout
    respond response

requestId :: Request -> RequestId
requestId request =
    RequestId $
        T.decodeUtf8With lenientDecode $
            fromMaybe "-" $
                lookup "X-Request-Id" $
                    requestHeaders request

waiResponseStatus :: Response -> Status
waiResponseStatus response =
    case response of
        ResponseFile status _ _ _ ->
            status
        ResponseBuilder status _ _ ->
            status
        ResponseStream status _ _ ->
            status
        ResponseRaw _ fallback ->
            waiResponseStatus fallback
