{-# LANGUAGE OverloadedStrings #-}

module Proxy.Antithesis.ServerSpec
    ( spec
    )
where

import Data.ByteString.Char8 qualified as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types
    ( hAuthorization
    , hContentType
    , status200
    )
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Network.HTTP.Types (parseQuery)
import Network.Wai (Application, Request (pathInfo, queryString, rawPathInfo, rawQueryString, requestHeaders, requestMethod), responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test
    ( SResponse (..)
    , defaultRequest
    , request
    , runSession
    )
import Proxy.Antithesis.Server
    ( UpstreamConfig (..)
    , makeServantApp
    )
import Servant.Client (parseBaseUrl)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "Proxy.Antithesis.Server (Servant)" $ do
        it "forwards GET /api/v0/runs?limit=&after= with Bearer API key" $
            check "/api/v0/runs?limit=5&after=foo"
                ("/api/v0/runs", "?limit=5&after=foo")

        it "forwards GET /api/v0/runs/{run_id}" $
            check "/api/v0/runs/abc-54-7"
                ("/api/v0/runs/abc-54-7", "")

        it "forwards GET /api/v0/runs/{run_id}/properties" $
            check "/api/v0/runs/abc-54-7/properties"
                ("/api/v0/runs/abc-54-7/properties", "")

        it "forwards GET /api/v0/runs/{run_id}/events?q=" $
            check "/api/v0/runs/abc-54-7/events?q=getDir"
                ("/api/v0/runs/abc-54-7/events", "?q=getDir")

        it "forwards GET /api/v0/runs/{run_id}/logs?input_hash=&vtime=" $
            check "/api/v0/runs/abc-54-7/logs?input_hash=0&vtime=0"
                ("/api/v0/runs/abc-54-7/logs", "?input_hash=0&vtime=0")

        it "forwards GET /api/v0/runs/{run_id}/build_logs" $
            check "/api/v0/runs/abc-54-7/build_logs"
                ("/api/v0/runs/abc-54-7/build_logs", "")

check :: BC.ByteString -> (BC.ByteString, BC.ByteString) -> IO ()
check incoming (expectedPath, expectedQuery) = do
    seenRef <- newIORef Nothing
    response <-
        withUpstream (captureUpstream seenRef "[]") $ \baseUrl ->
            runServant baseUrl (BC.unpack incoming)
    simpleStatus response `shouldBe` status200
    seen <- readIORef seenRef
    seen
        `shouldBe` Just
            ( expectedPath
            , expectedQuery
            , Just "Bearer the-key"
            )

runServant :: String -> String -> IO SResponse
runServant baseUrl rawTarget = do
    manager <- newManager defaultManagerSettings
    parsedBase <- parseBaseUrl baseUrl
    let cfg =
            UpstreamConfig
                { upstreamBaseUrl = parsedBase
                , upstreamApiKey = "the-key"
                , upstreamManager = manager
                }
        app = makeServantApp cfg
        (path, query) = break (== '?') rawTarget
    let qbs = BC.pack query
        pbs = BC.pack path
    runSession
        ( request
            defaultRequest
                { requestMethod = "GET"
                , rawPathInfo = pbs
                , pathInfo = splitPath pbs
                , rawQueryString = qbs
                , queryString = parseQuery qbs
                }
        )
        app

splitPath :: BC.ByteString -> [Text]
splitPath = fmap T.decodeUtf8 . filter (not . BC.null) . BC.split '/'

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
    respond $ responseLBS status200 [(hContentType, "text/plain")] $ BC.fromStrict body
