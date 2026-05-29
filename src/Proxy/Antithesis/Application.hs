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
import Data.Time.Clock (getCurrentTime)
import Lib.GitHub.Auth.Identify (whoami)
import Lib.GitHub.Auth.TeamCheck (checkTeamMembership)
import Network.HTTP.Client
    ( Manager
    , Request (method, requestHeaders)
    , httpLbs
    , newManager
    , parseRequest
    , responseStatus
    )
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
    , Request (pathInfo, requestMethod)
    , Response
    , responseLBS
    )
import Proxy.Antithesis.Cache (newMembershipCache)
import Proxy.Antithesis.Config
    ( Settings (..)
    )
import Proxy.Antithesis.Handler.Runs
    ( RunsConfig (..)
    , runsHandler
    )
import Proxy.Antithesis.Middleware.Auth
    ( AuthConfig (..)
    , authMiddleware
    )

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
        ("GET", ["healthz"]) ->
            respond $ plain status200 "ok"
        ("GET", ["readyz"]) -> do
            ready <- readinessCheck $ proxyReadinessConfig config
            respond $
                if ready
                    then plain status200 "ready"
                    else plain status503 "not ready"
        ("GET", ["api", "v1", "runs"]) ->
            authMiddleware
                (proxyAuthConfig config)
                (runsHandler $ proxyRunsConfig config)
                request
                respond
        _ ->
            respond $ plain status404 "not found"

readinessCheck :: ReadinessConfig -> IO Bool
readinessCheck config =
    (&&)
        <$> readinessAntithesis config
        <*> readinessGitHub config

productionApplication :: Settings -> IO Application
productionApplication settings = do
    manager <- newManager tlsManagerSettings
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

productionReadinessConfig :: Settings -> Manager -> ReadinessConfig
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

probeEndpoint :: Manager -> String -> RequestHeaders -> IO Bool
probeEndpoint manager url headers = do
    result <- try @SomeException $ do
        baseRequest <- parseRequest url
        response <-
            httpLbs
                baseRequest
                    { method = methodGet
                    , requestHeaders = headers
                    }
                manager
        pure $ statusCode (responseStatus response) < 500
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
