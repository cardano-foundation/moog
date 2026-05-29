{-# LANGUAGE OverloadedStrings #-}

module Proxy.Antithesis.ApplicationSpec
    ( spec
    )
where

import Data.ByteString.Char8 qualified as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Lib.GitHub.Auth.DeviceFlow (OAuthToken)
import Lib.GitHub.Auth.Identify (Login (..))
import Lib.GitHub.Auth.TeamCheck
    ( MembershipResult (..)
    , Org (..)
    , TeamSlug (..)
    )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types
    ( RequestHeaders
    , hAuthorization
    , parseQuery
    , status200
    , status401
    , status503
    )
import Network.Wai
    ( Application
    , Request (pathInfo, queryString, rawPathInfo, rawQueryString, requestHeaders, requestMethod)
    , responseLBS
    )
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test
    ( SResponse (..)
    , defaultRequest
    , request
    , runSession
    )
import Proxy.Antithesis.Application
    ( ProxyApplicationConfig (..)
    , ReadinessConfig (..)
    , proxyApplication
    )
import Proxy.Antithesis.Cache (newMembershipCache)
import Proxy.Antithesis.Middleware.Auth (AuthConfig (..))
import Proxy.Antithesis.Server
    ( UpstreamConfig (..)
    , makeServantApp
    )
import Servant.Client (parseBaseUrl)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "Proxy.Antithesis.Application" $ do
        it "serves health without auth" $ do
            response <- runProxyApp True True (requestFor "/healthz" [])

            simpleStatus response `shouldBe` status200
            simpleBody response `shouldBe` "ok"

        it "serves readiness only when Antithesis and GitHub are reachable" $ do
            ready <- runProxyApp True True (requestFor "/readyz" [])
            notReady <- runProxyApp True False (requestFor "/readyz" [])

            simpleStatus ready `shouldBe` status200
            simpleBody ready `shouldBe` "ready"
            simpleStatus notReady `shouldBe` status503
            simpleBody notReady `shouldBe` "not ready"

        it "rejects unauthenticated requests for any non-public path" $ do
            -- /missing is not in the Servant API. Auth runs first; without
            -- a token, the proxy returns 401 (we don't leak whether the
            -- path would have matched).
            response <- runProxyApp True True (requestFor "/missing" [])

            simpleStatus response `shouldBe` status401

        it "protects and routes GET /api/v0/runs" $ do
            seenRef <- newIORef Nothing
            response <-
                withUpstream (captureUpstream seenRef "[1,2,3]") $ \baseUrl ->
                    runProxyAppWithUpstream
                        baseUrl
                        ( requestFor
                            "/api/v0/runs"
                            [(hAuthorization, "Bearer gh-token")]
                        )
                            { rawQueryString = "?limit=2"
                            , queryString = parseQuery "?limit=2"
                            }

            simpleStatus response `shouldBe` status200
            simpleBody response `shouldBe` "[1,2,3]"
            seen <- readIORef seenRef
            seen
                `shouldBe` Just
                    ( "/api/v0/runs"
                    , "?limit=2"
                    , Just "Bearer test-api-key"
                    )

        it "applies auth to GET /api/v0/runs" $ do
            response <- runProxyApp True True (requestFor "/api/v0/runs" [])

            simpleStatus response `shouldBe` status401

runProxyApp :: Bool -> Bool -> Network.Wai.Request -> IO SResponse
runProxyApp antithesisReady githubReady request' =
    withUpstream (\_ respond -> respond $ responseLBS status200 [] "[]") $ \baseUrl ->
        runProxyAppWithReadiness baseUrl antithesisReady githubReady request'

runProxyAppWithUpstream :: String -> Network.Wai.Request -> IO SResponse
runProxyAppWithUpstream baseUrl =
    runProxyAppWithReadiness baseUrl True True

runProxyAppWithReadiness
    :: String
    -> Bool
    -> Bool
    -> Network.Wai.Request
    -> IO SResponse
runProxyAppWithReadiness baseUrl antithesisReady githubReady request' = do
    application <- testApplication baseUrl antithesisReady githubReady
    runSession (request request') application

testApplication :: String -> Bool -> Bool -> IO Application
testApplication baseUrl antithesisReady githubReady = do
    manager <- newManager defaultManagerSettings
    cache <- newMembershipCache 60
    parsedBase <- parseBaseUrl baseUrl
    let cfg =
            UpstreamConfig
                { upstreamBaseUrl = parsedBase
                , upstreamApiKey = "test-api-key"
                , upstreamManager = manager
                }
        servantApp = makeServantApp cfg
    pure $
        proxyApplication
            ProxyApplicationConfig
                { proxyAuthConfig =
                    AuthConfig
                        { authOrg = Org "pragma-org"
                        , authTeam = TeamSlug "antithesis-access"
                        , authCache = cache
                        , authNow = pure baseTime
                        , authWhoami = const $ pure $ Right $ Login "octocat"
                        , authCheckTeamMembership = activeMembership
                        }
                , proxyServantApp = servantApp
                , proxyReadinessConfig =
                    ReadinessConfig
                        { readinessAntithesis = pure antithesisReady
                        , readinessGitHub = pure githubReady
                        }
                }

requestFor :: BC.ByteString -> RequestHeaders -> Network.Wai.Request
requestFor path headers =
    defaultRequest
        { requestMethod = "GET"
        , rawPathInfo = path
        , pathInfo = splitPath path
        , requestHeaders = headers
        }

splitPath :: BC.ByteString -> [Text]
splitPath =
    fmap decodeSegment . filter (not . BC.null) . BC.split '/'

decodeSegment :: BC.ByteString -> Text
decodeSegment =
    T.decodeUtf8

activeMembership
    :: OAuthToken
    -> Org
    -> TeamSlug
    -> Login
    -> IO MembershipResult
activeMembership _ _ _ _ =
    pure Active

withUpstream :: Application -> (String -> IO a) -> IO a
withUpstream upstream action =
    testWithApplication (pure upstream) $ \port ->
        action $ "http://127.0.0.1:" <> show port

captureUpstream
    :: IORef (Maybe (BC.ByteString, BC.ByteString, Maybe BC.ByteString))
    -> BC.ByteString
    -> Application
captureUpstream seenRef body request' respond = do
    writeIORef
        seenRef
        $ Just
            ( rawPathInfo request'
            , rawQueryString request'
            , lookup hAuthorization $ requestHeaders request'
            )
    respond $ responseLBS status200 [("content-type", "text/plain")] $ BC.fromStrict body

baseTime :: UTCTime
baseTime = UTCTime (toEnum 0) (secondsToDiffTime 0)
