{-# LANGUAGE OverloadedStrings #-}

module Proxy.Antithesis.Handler.RunsSpec
    ( spec
    )
where

import Data.ByteString.Char8 qualified as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types
    ( hAuthorization
    , hContentLength
    , hContentType
    , Status
    , status200
    , status503
    )
import Network.Wai
    ( Application
    , Request (rawQueryString, requestHeaders)
    , responseLBS
    )
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test
    ( SResponse (..)
    , defaultRequest
    , request
    , runSession
    )
import Proxy.Antithesis.Handler.Runs
    ( RunsConfig (..)
    , runsHandler
    )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "Proxy.Antithesis.Handler.Runs" $ do
        it "forwards GET /api/v1/runs with query and basic auth" $ do
            seenRef <- newIORef Nothing
            response <-
                withUpstream
                    (captureRuns seenRef status200)
                    $ \baseUrl -> runProxy baseUrl

            simpleStatus response `shouldBe` status200
            simpleBody response `shouldBe` "runs-body"
            lookup hContentType (simpleHeaders response)
                `shouldBe` Just "application/json"
            lookup hContentLength (simpleHeaders response)
                `shouldBe` Just "9"
            seen <- readIORef seenRef
            seen
                `shouldBe` Just
                    ( "?limit=5&cursor=a%2Fb"
                    , Just "Basic cHJhZ21hOnNlY3JldA=="
                    )

        it "sanitizes upstream 5xx bodies" $ do
            response <-
                withUpstream
                    (\_ respond -> respond $ responseLBS status503 [] "secret upstream body")
                    $ \baseUrl -> runProxy baseUrl

            simpleStatus response `shouldBe` status503
            simpleBody response `shouldBe` "upstream request failed"

runProxy :: String -> IO SResponse
runProxy baseUrl = do
    manager <- newManager defaultManagerSettings
    runSession
        ( request
            defaultRequest
                { rawQueryString = "?limit=5&cursor=a%2Fb"
                }
        )
        $ runsHandler
            RunsConfig
                { runsAntithesisUrl = baseUrl
                , runsAntithesisUser = "pragma"
                , runsAntithesisPassword = "secret"
                , runsManager = manager
                }

withUpstream :: Application -> (String -> IO a) -> IO a
withUpstream upstream action =
    testWithApplication (pure upstream) $ \port ->
        action $ "http://127.0.0.1:" <> show port

captureRuns
    :: IORef (Maybe (BC.ByteString, Maybe BC.ByteString))
    -> Status
    -> Application
captureRuns seenRef status request' respond = do
    writeIORef
        seenRef
        $ Just
            ( rawQueryString request'
            , lookup hAuthorization $ requestHeaders request'
            )
    respond $
        responseLBS
            status
            [(hContentType, "application/json"), (hContentLength, "9")]
            "runs-body"
