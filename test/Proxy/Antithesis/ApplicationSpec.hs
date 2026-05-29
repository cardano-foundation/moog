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
    , status200
    , status401
    , status404
    , status503
    )
import Network.Wai
    ( Application
    , Request (pathInfo, rawPathInfo, rawQueryString, requestHeaders, requestMethod)
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
import Proxy.Antithesis.Handler.Runs (RunsConfig (..))
import Proxy.Antithesis.Middleware.Auth (AuthConfig (..))
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

        it "returns 404 for unknown routes" $ do
            response <- runProxyApp True True (requestFor "/missing" [])

            simpleStatus response `shouldBe` status404
            simpleBody response `shouldBe` "not found"

        it "protects and routes GET /api/v1/runs" $ do
            seenRef <- newIORef Nothing
            response <-
                withUpstream (captureRuns seenRef) $ \baseUrl ->
                    runProxyAppWithUpstream
                        baseUrl
                        ( requestFor
                            "/api/v1/runs"
                            [(hAuthorization, "Bearer gh-token")]
                        )
                            { rawQueryString = "?limit=2"
                            }

            simpleStatus response `shouldBe` status200
            simpleBody response `shouldBe` "runs-body"
            seen <- readIORef seenRef
            seen
                `shouldBe` Just
                    ( "?limit=2"
                    , Just "Basic cHJhZ21hOnNlY3JldA=="
                    )

        it "applies auth to GET /api/v1/runs" $ do
            response <- runProxyApp True True (requestFor "/api/v1/runs" [])

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
                , proxyRunsConfig =
                    RunsConfig
                        { runsAntithesisUrl = baseUrl
                        , runsAntithesisUser = "pragma"
                        , runsAntithesisPassword = "secret"
                        , runsManager = manager
                        }
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

captureRuns
    :: IORef (Maybe (BC.ByteString, Maybe BC.ByteString))
    -> Application
captureRuns seenRef request' respond = do
    writeIORef
        seenRef
        $ Just
            ( rawQueryString request'
            , lookup hAuthorization $ requestHeaders request'
            )
    respond $ responseLBS status200 [] "runs-body"

baseTime :: UTCTime
baseTime = UTCTime (toEnum 0) (secondsToDiffTime 0)
