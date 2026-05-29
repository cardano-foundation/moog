{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | WAI application assembly for the Antithesis proxy daemon.
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
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
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
    , status404
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
import Proxy.Antithesis.Cache (newMembershipCache)
import Proxy.Antithesis.Config
    ( Settings (..)
    )
import Proxy.Antithesis.Audit
    ( AuditEvent (..)
    , RequestId (..)
    , renderAuditEvent
    )
import Proxy.Antithesis.Handler.Runs
    ( RunsConfig (..)
    , runsHandler
    )
import Proxy.Antithesis.Middleware.Auth
    ( AuthConfig (..)
    , authMiddleware
    , authorizedLoginKey
    )
import System.IO (hFlush, stdout)

data ProxyApplicationConfig = ProxyApplicationConfig
    { proxyAuthConfig :: AuthConfig
    , proxyRunsConfig :: RunsConfig
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
        ("GET", ["api", "v1", "runs"]) -> do
            authHandled <- newIORef False
            start <- getCurrentTime
            authMiddleware
                (proxyAuthConfig config)
                ( \authorizedRequest authorizedRespond -> do
                    writeIORef authHandled True
                    runsHandler
                        (proxyRunsConfig config)
                        authorizedRequest
                        $ \response ->
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
        _ -> do
            start <- getCurrentTime
            auditAndRespond request start Nothing (plain status404 "not found") respond

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
                , proxyRunsConfig =
                    RunsConfig
                        { runsAntithesisUrl = settingsAntithesisUrl settings
                        , runsAntithesisUser = settingsAntithesisUser settings
                        , runsAntithesisPassword =
                            settingsAntithesisPassword settings
                        , runsManager = manager
                        }
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
                [(hAuthorization, antithesisBasicAuthorization settings)]
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
    stripTrailingSlash (settingsAntithesisUrl settings) <> "/api/v1/runs"

antithesisBasicAuthorization :: Settings -> ByteString
antithesisBasicAuthorization settings =
    "Basic "
        <> Base64.encode
            ( settingsAntithesisUser settings
                <> ":"
                <> settingsAntithesisPassword settings
            )

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
