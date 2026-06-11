{-# LANGUAGE OverloadedStrings #-}

module User.Agent.Antithesis.StateSpec
    ( spec
    )
where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import User.Agent.Antithesis.State
    ( AntithesisRun (..)
    , AntithesisRunStatus (..)
    , PendingDecision (..)
    , PropertiesPage (..)
    , RunningDecision (..)
    , RunProperty (..)
    , RunsPage (..)
    , canonicalRun
    , matchingRuns
    , parsePropertiesPage
    , parseRunsPage
    , pendingDecision
    , runFailedAssertions
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

        it "tie-breaks equal-status duplicates by ascending run id" $ do
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

            canonicalRun [runB, runA] `shouldBe` runA

        it "prefers the completed run over an incomplete duplicate" $ do
            let runA =
                    run
                        "run-a"
                        RunIncomplete
                        (Just matchingDescription)
                        (Just "https://report.example/run-a")
                runB =
                    run
                        "run-b"
                        RunCompleted
                        (Just matchingDescription)
                        (Just "https://report.example/run-b")

            canonicalRun [runA, runB] `shouldBe` runB

        it "drains duplicate pending matches by accepting the canonical run" $ do
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
                `shouldBe` PendingAcceptDuplicate runA [runA, runB]

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

        it "drains duplicate running matches by finishing from the canonical run" $ do
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
                `shouldBe` RunningFinishDuplicate
                    runA
                    OutcomeSuccess
                    (URL "https://report.example/run-a")
                    [runA, runB]

        it "finishes from the completed run even when a duplicate is still running" $ do
            let runA =
                    run
                        "run-a"
                        RunInProgress
                        (Just matchingDescription)
                        Nothing
                runB =
                    run
                        "run-b"
                        RunCompleted
                        (Just matchingDescription)
                        (Just "https://report.example/run-b")

            runningDecision testRun [runA, runB]
                `shouldBe` RunningFinishDuplicate
                    runB
                    OutcomeSuccess
                    (URL "https://report.example/run-b")
                    [runA, runB]

        it "finishes from the completed run when a duplicate is incomplete (prod #175)" $ do
            let runA =
                    run
                        "run-a"
                        RunIncomplete
                        (Just matchingDescription)
                        (Just "https://report.example/run-a")
                runB =
                    run
                        "run-b"
                        RunCompleted
                        (Just matchingDescription)
                        (Just "https://report.example/run-b")

            runningDecision testRun [runA, runB]
                `shouldBe` RunningFinishDuplicate
                    runB
                    OutcomeSuccess
                    (URL "https://report.example/run-b")
                    [runA, runB]

        -- Live-derived regression: the real /api/v0/runs payload captured on
        -- 2026-05-30 for the stuck Leios run (#138). The fixture has status
        -- "incomplete" and NO "links" key at all (not even "links": {}). This
        -- also exercises the real matching path: leiosTestRun must hash to the
        -- on-chain testRunId 42030238… and render to the run's description.
        it "finishes the live Leios incomplete run with no triage report" $ do
            decoded <-
                Aeson.eitherDecodeFileStrict
                    "test/data/138-leios-incomplete-no-report.json"
            value <- either (fail . ("fixture JSON: " <>)) pure decoded
            page <- either (fail . ("parseRunsPage: " <>)) pure (parseRunsPage value)
            case runsPageRuns page of
                [observed] ->
                    runningDecision leiosTestRun [observed]
                        `shouldBe` RunningFinish
                            observed
                            OutcomeFailure
                            ( URL
                                "antithesis://runs/4720e2cc2382cf1c8581b9da13cdd26d-54-7/no-triage-report"
                            )
                other -> expectationFailure $ "expected one run, got " <> show other

        it "treats a failing non-excluded assertion as a run failure" $ do
            let props =
                    [ RunProperty "always: ledger valid" False False
                    , RunProperty
                        "Unexpected terminations: state_timed_out"
                        True
                        False
                    ]
            runFailedAssertions props `shouldBe` True

        it "treats a failing event SUT check as a run failure" $ do
            -- Events now count: a failing event-typed SUT check (an
            -- `asteria-game/*.sh` command, exit code 1, …) is a real
            -- failure, not platform coverage noise.
            let props =
                    [ RunProperty
                        "asteria-game/eventually_alive.sh"
                        True
                        True
                    , RunProperty "always: ledger valid" False False
                    ]
            runFailedAssertions props `shouldBe` True

        it "ignores failing platform-excluded properties" $ do
            -- A failing platform property (assert or event) on the
            -- upstream-escalation list does not by itself flip the run.
            let props =
                    [ RunProperty "Unique Edges" True False
                    , RunProperty "Sometimes: Root moments" True True
                    , RunProperty "always: ledger valid" False False
                    ]
            runFailedAssertions props `shouldBe` False

        it "passes when every assertion passes" $ do
            let props =
                    [ RunProperty "always: ledger valid" False False
                    , RunProperty "Sometimes: fork reached" False True
                    ]
            runFailedAssertions props `shouldBe` False

        it "parses a properties page with a pagination cursor" $ do
            parsePropertiesPage
                ( propertiesPageJson
                    [ propertyJson "always: ledger valid" "Passing" False
                    , propertyJson "eventually_converged" "Failing" False
                    ]
                    (Just "cursor-2")
                )
                `shouldBe` Right
                    PropertiesPage
                        { propsPageData =
                            [ RunProperty
                                { propName = "always: ledger valid"
                                , propFailing = False
                                , propIsEvent = False
                                }
                            , RunProperty
                                { propName = "eventually_converged"
                                , propFailing = True
                                , propIsEvent = False
                                }
                            ]
                        , propsPageNextCursor = Just "cursor-2"
                        }

-- | Reconstructed on-chain test-run for the live Leios fixture (#138). Its
-- fields must hash (via mkTestRunId) to testRunId 42030238… so matchingRuns
-- pairs it with the captured Antithesis run.
leiosTestRun :: TestRun
leiosTestRun =
    TestRun
        { platform = Platform "github"
        , repository = GithubRepository "input-output-hk" "ouroboros-leios"
        , directory = Directory "antithesis/testnets/leios-devnet"
        , commitId = Commit "76f3a47d8c236b8dd208c92593be54d9fe347b1e"
        , tryIndex = Try 1
        , requester = GithubUsername "wolf31o2"
        }

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

propertiesPageJson :: [Aeson.Value] -> Maybe Text -> Aeson.Value
propertiesPageJson props nextCursor =
    Aeson.object
        [ "data" Aeson..= props
        , "next_cursor" Aeson..= nextCursor
        ]

propertyJson :: Text -> Text -> Bool -> Aeson.Value
propertyJson name status isEvent =
    Aeson.object
        [ "name" Aeson..= name
        , "status" Aeson..= status
        , "is_event" Aeson..= isEvent
        ]
