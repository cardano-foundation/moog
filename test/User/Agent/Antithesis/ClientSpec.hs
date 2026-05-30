{-# LANGUAGE OverloadedStrings #-}

module User.Agent.Antithesis.ClientSpec
    ( spec
    )
where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import Data.Text (Text)
import Network.HTTP.Types
    ( hAuthorization
    , queryToQueryText
    , status200
    )
import Network.Wai
    ( Application
    , Request (queryString, rawPathInfo, requestHeaders)
    , responseLBS
    )
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec (Spec, describe, it, shouldBe)
import User.Agent.Antithesis.Client
    ( AntithesisApiConfig (..)
    , AntithesisApiKey (..)
    , AntithesisApiUrl (..)
    , deriveAntithesisApiUrl
    , listAllRuns
    )
import User.Agent.Antithesis.State
    ( AntithesisRun (..)
    , AntithesisRunStatus (..)
    )
import User.Agent.PushTest (LaunchUrl (..))

spec :: Spec
spec = describe "User.Agent.Antithesis.Client" $ do
    it "derives the read API base URL from the launch URL" $
        deriveAntithesisApiUrl
            (LaunchUrl "https://tenant.antithesis.com/api/v1/launch/cardano")
            `shouldBe` AntithesisApiUrl "https://tenant.antithesis.com"

    it "lists all runs with Bearer auth and cursor pagination" $ do
        seenRef <- newIORef []
        result <-
            withRunsApi seenRef $ \baseUrl ->
                listAllRuns
                    AntithesisApiConfig
                        { antithesisApiUrl = AntithesisApiUrl baseUrl
                        , antithesisApiKey = AntithesisApiKey "api-key"
                        }

        result
            `shouldBe` Right
                [ AntithesisRun
                    { antithesisRunId = "run-1"
                    , antithesisRunStatus = RunInProgress
                    , antithesisRunDescription = Just "description-1"
                    , antithesisRunTriageReport = Nothing
                    }
                , AntithesisRun
                    { antithesisRunId = "run-2"
                    , antithesisRunStatus = RunCompleted
                    , antithesisRunDescription = Just "description-2"
                    , antithesisRunTriageReport =
                        Just "https://report.example/run-2"
                    }
                ]
        seen <- readIORef seenRef
        seen
            `shouldBe`
                [ ( "/api/v0/runs"
                  , [("limit", Just "100")]
                  , Just "Bearer api-key"
                  )
                , ( "/api/v0/runs"
                  , [("limit", Just "100"), ("cursor", Just "cursor-2")]
                  , Just "Bearer api-key"
                  )
                ]

type SeenRequest = (ByteString, [(Text, Maybe Text)], Maybe ByteString)

withRunsApi :: IORef [SeenRequest] -> (String -> IO a) -> IO a
withRunsApi seenRef action =
    testWithApplication (pure $ runsApi seenRef) $ \port ->
        action $ "http://127.0.0.1:" <> show port

runsApi :: IORef [SeenRequest] -> Application
runsApi seenRef request respond = do
    let queryText = queryToQueryText $ queryString request
        cursor = lookup "cursor" queryText
        auth = lookup hAuthorization $ requestHeaders request
    appendSeen seenRef (rawPathInfo request, queryText, auth)
    respond $
        responseLBS
            status200
            [("Content-Type", "application/json")]
            $ Aeson.encode
            $ case cursor of
                Nothing ->
                    runsPageJson
                        [runJson "run-1" "in_progress" "description-1" Nothing]
                        (Just "cursor-2")
                Just (Just "cursor-2") ->
                    runsPageJson
                        [ runJson
                            "run-2"
                            "completed"
                            "description-2"
                            (Just "https://report.example/run-2")
                        ]
                        Nothing
                _ ->
                    runsPageJson [] Nothing

appendSeen :: IORef [SeenRequest] -> SeenRequest -> IO ()
appendSeen ref req =
    atomicModifyIORef' ref $ \seen -> (seen <> [req], ())

runsPageJson :: [Aeson.Value] -> Maybe Text -> Aeson.Value
runsPageJson runs nextCursor =
    Aeson.object
        [ "data" Aeson..= runs
        , "next_cursor" Aeson..= nextCursor
        ]

runJson :: Text -> Text -> Text -> Maybe Text -> Aeson.Value
runJson runId status description report =
    Aeson.object
        [ "run_id" Aeson..= runId
        , "status" Aeson..= status
        , "parameters"
            Aeson..= Aeson.object
                [ "antithesis.description" Aeson..= description
                ]
        , "links"
            Aeson..= maybe
                Aeson.Null
                ( \url ->
                    Aeson.object
                        [ "triage_report" Aeson..= url
                        ]
                )
                report
        ]
