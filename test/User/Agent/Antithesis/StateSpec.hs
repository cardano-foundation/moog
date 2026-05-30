{-# LANGUAGE OverloadedStrings #-}

module User.Agent.Antithesis.StateSpec
    ( spec
    )
where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe)
import User.Agent.Antithesis.State
    ( AntithesisRun (..)
    , AntithesisRunStatus (..)
    , PendingDecision (..)
    , RunningDecision (..)
    , RunsPage (..)
    , matchingRuns
    , parseRunsPage
    , pendingDecision
    , runningDecision
    )
import User.Agent.PushTest (renderTestRun)
import User.Agent.Types (mkTestRunId)
import User.Types
    ( Outcome (..)
    , TestRun (..)
    , URL (..)
    )
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , GithubRepository (..)
    , GithubUsername (..)
    , Platform (..)
    , Try (..)
    )

spec :: Spec
spec =
    describe "User.Agent.Antithesis.State" $ do
        it "parses the documented runs page shape" $ do
            parseRunsPage
                ( runsPageJson
                    [ runJson
                        "run-1"
                        "in_progress"
                        (Just matchingDescription)
                        (Just "https://report.example/run-1")
                    ]
                    (Just "cursor-2")
                )
                `shouldBe` Right
                    RunsPage
                        { runsPageRuns =
                            [ AntithesisRun
                                { antithesisRunId = "run-1"
                                , antithesisRunStatus = RunInProgress
                                , antithesisRunDescription =
                                    Just matchingDescription
                                , antithesisRunTriageReport =
                                    Just "https://report.example/run-1"
                                }
                            ]
                        , runsPageNextCursor = Just "cursor-2"
                        }

        it "matches runs by deterministic antithesis.description" $ do
            let matching =
                    run
                        "run-1"
                        RunInProgress
                        (Just matchingDescription)
                        Nothing
                other =
                    run
                        "run-2"
                        RunInProgress
                        (Just "other-description")
                        Nothing

            matchingRuns testRun [other, matching] `shouldBe` [matching]

        it "launches a pending test only when no matching run is visible" $ do
            pendingDecision testRun [] `shouldBe` PendingLaunch

        it "accepts a pending test from a later API observation" $ do
            let observed =
                    run
                        "run-1"
                        RunInProgress
                        (Just matchingDescription)
                        Nothing

            pendingDecision testRun [observed]
                `shouldBe` PendingAccept observed

        it "does not repost or mutate on duplicate pending matches" $ do
            let runA =
                    run
                        "run-a"
                        RunStarting
                        (Just matchingDescription)
                        Nothing
                runB =
                    run
                        "run-b"
                        RunInProgress
                        (Just matchingDescription)
                        Nothing

            pendingDecision testRun [runA, runB]
                `shouldBe` PendingDuplicate [runA, runB]

        it "finishes a running test from a completed API run with report URL" $ do
            let observed =
                    run
                        "run-1"
                        RunCompleted
                        (Just matchingDescription)
                        (Just "https://report.example/run-1")

            runningDecision testRun [observed]
                `shouldBe` RunningFinish
                    observed
                    OutcomeSuccess
                    (URL "https://report.example/run-1")

        it "maps incomplete terminal runs to failure" $ do
            let observed =
                    run
                        "run-1"
                        RunIncomplete
                        (Just matchingDescription)
                        (Just "https://report.example/run-1")

            runningDecision testRun [observed]
                `shouldBe` RunningFinish
                    observed
                    OutcomeFailure
                    (URL "https://report.example/run-1")

        it "stays conservative on completed runs without a report URL" $ do
            -- Success is never attested without the report link: a terminal
            -- `completed` run missing `links.triage_report` keeps waiting.
            let observed =
                    run
                        "run-1"
                        RunCompleted
                        (Just matchingDescription)
                        Nothing

            runningDecision testRun [observed] `shouldBe` RunningWait

        it "finishes incomplete runs without a report as failure" $ do
            let observed =
                    run
                        "run-1"
                        RunIncomplete
                        (Just matchingDescription)
                        Nothing

            runningDecision testRun [observed]
                `shouldBe` RunningFinish
                    observed
                    OutcomeFailure
                    (URL "antithesis://runs/run-1/no-triage-report")

        it "finishes cancelled runs without a report as failure" $ do
            let observed =
                    run
                        "run-1"
                        RunCancelled
                        (Just matchingDescription)
                        Nothing

            runningDecision testRun [observed]
                `shouldBe` RunningFinish
                    observed
                    OutcomeFailure
                    (URL "antithesis://runs/run-1/no-triage-report")

        it "keeps waiting on non-terminal runs without a report" $ do
            let observed =
                    run
                        "run-1"
                        RunInProgress
                        (Just matchingDescription)
                        Nothing

            runningDecision testRun [observed] `shouldBe` RunningWait

        it "does not choose silently between duplicate running matches" $ do
            let runA =
                    run
                        "run-a"
                        RunCompleted
                        (Just matchingDescription)
                        (Just "https://report.example/run-a")
                runB =
                    run
                        "run-b"
                        RunCompleted
                        (Just matchingDescription)
                        (Just "https://report.example/run-b")

            runningDecision testRun [runA, runB]
                `shouldBe` RunningDuplicate [runA, runB]

testRun :: TestRun
testRun =
    TestRun
        { platform = Platform "github"
        , repository = GithubRepository "cardano-foundation" "moog"
        , directory = Directory "compose/testnets/cardano_node_master"
        , commitId = Commit "abcdef1234567890"
        , tryIndex = Try 1
        , requester = GithubUsername "cfhal"
        }

matchingDescription :: Text
matchingDescription =
    T.pack $ renderTestRun (mkTestRunId testRun) testRun

run
    :: Text
    -> AntithesisRunStatus
    -> Maybe Text
    -> Maybe Text
    -> AntithesisRun
run runId status description report =
    AntithesisRun
        { antithesisRunId = runId
        , antithesisRunStatus = status
        , antithesisRunDescription = description
        , antithesisRunTriageReport = report
        }

runsPageJson :: [Aeson.Value] -> Maybe Text -> Aeson.Value
runsPageJson runs nextCursor =
    Aeson.object
        [ "data" Aeson..= runs
        , "next_cursor" Aeson..= nextCursor
        ]

runJson :: Text -> Text -> Maybe Text -> Maybe Text -> Aeson.Value
runJson runId status description report =
    Aeson.object
        [ "run_id" Aeson..= runId
        , "status" Aeson..= status
        , "parameters"
            Aeson..= maybe
                Aeson.Null
                ( \desc ->
                    Aeson.object
                        [ "antithesis.description" Aeson..= desc
                        ]
                )
                description
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
