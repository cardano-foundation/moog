{-# LANGUAGE OverloadedStrings #-}

module User.Agent.Antithesis.PlanSpec
    ( spec
    )
where

import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , Duration (..)
    , FaultsEnabled (..)
    , GithubRepository (..)
    , GithubUsername (..)
    , HasInstrumentation (..)
    , Platform (..)
    , Try (..)
    )
import Core.Types.Fact (Fact (..), Slot (..))
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe)
import User.Agent.Antithesis.Plan
    ( PendingAction (..)
    , PollPlan (..)
    , RunningAction (..)
    , planAgentPoll
    )
import User.Agent.Antithesis.State
    ( AntithesisRun (..)
    , AntithesisRunStatus (..)
    )
import User.Agent.PushTest (renderTestRun)
import User.Agent.Types (mkTestRunId)
import User.Types
    ( Outcome (..)
    , Phase (..)
    , TestRun (..)
    , TestRunState (..)
    , URL (..)
    )

spec :: Spec
spec =
    describe "User.Agent.Antithesis.Plan" $ do
        it "launches a trusted pending test without accepting it in the same poll" $
            planAgentPoll trusted mempty [] [pendingFact] []
                `shouldBe` PollPlan
                    { pendingActions = [PendingLaunchOnly pendingFact]
                    , runningActions = []
                    }

        it "launches a pending test whose description is not yet marked" $
            planAgentPoll trusted mempty [] [pendingFact] []
                `shouldBe` PollPlan
                    { pendingActions = [PendingLaunchOnly pendingFact]
                    , runningActions = []
                    }

        it "awaits observation for an already-marked pending test not yet visible" $
            planAgentPoll
                trusted
                (Set.singleton matchingDescription)
                []
                [pendingFact]
                []
                `shouldBe` PollPlan
                    { pendingActions = [PendingAwaitObservation pendingFact]
                    , runningActions = []
                    }

        it "accepts a pending test only from a matching API observation" $ do
            let observed =
                    run
                        "run-1"
                        RunInProgress
                        (Just matchingDescription)
                        Nothing

            planAgentPoll trusted mempty [observed] [pendingFact] []
                `shouldBe` PollPlan
                    { pendingActions = [PendingAcceptObserved pendingFact observed]
                    , runningActions = []
                    }

        it "drains duplicate pending API matches by accepting the canonical run" $ do
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

            planAgentPoll trusted mempty [runA, runB] [pendingFact] []
                `shouldBe` PollPlan
                    { pendingActions =
                        [PendingDrainDuplicate pendingFact runA [runA, runB]]
                    , runningActions = []
                    }

        it "finishes a running test from a terminal API run with report URL" $ do
            let observed =
                    run
                        "run-1"
                        RunCompleted
                        (Just matchingDescription)
                        (Just "https://report.example/run-1")

            planAgentPoll trusted mempty [observed] [] [runningFact]
                `shouldBe` PollPlan
                    { pendingActions = []
                    , runningActions =
                        [ RunningFinishObserved
                            runningFact
                            observed
                            OutcomeSuccess
                            (URL "https://report.example/run-1")
                        ]
                    }

        it "drains duplicate running API matches by finishing from the canonical run" $ do
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

            planAgentPoll trusted mempty [runA, runB] [] [runningFact]
                `shouldBe` PollPlan
                    { pendingActions = []
                    , runningActions =
                        [ RunningDrainFinish
                            runningFact
                            runA
                            OutcomeSuccess
                            (URL "https://report.example/run-a")
                            [runA, runB]
                        ]
                    }

        it "waits on duplicate running matches when the canonical run is not yet terminal" $ do
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

            planAgentPoll trusted mempty [runA, runB] [] [runningFact]
                `shouldBe` PollPlan
                    { pendingActions = []
                    , runningActions =
                        [RunningDrainWait runningFact runA [runA, runB]]
                    }

        it "pending and running drains pick the same canonical from an unordered match list" $ do
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
                unordered = [runB, runA]

            planAgentPoll trusted mempty unordered [pendingFact] []
                `shouldBe` PollPlan
                    { pendingActions =
                        [PendingDrainDuplicate pendingFact runA unordered]
                    , runningActions = []
                    }
            planAgentPoll trusted mempty unordered [] [runningFact]
                `shouldBe` PollPlan
                    { pendingActions = []
                    , runningActions =
                        [ RunningDrainFinish
                            runningFact
                            runA
                            OutcomeSuccess
                            (URL "https://report.example/run-a")
                            unordered
                        ]
                    }

trusted :: GithubUsername -> Bool
trusted _ = True

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

pendingFact :: Fact TestRun (TestRunState 'PendingT)
pendingFact =
    Fact testRun pendingState (Slot 1)

runningFact :: Fact TestRun (TestRunState 'RunningT)
runningFact =
    Fact testRun (Accepted pendingState) (Slot 2)

pendingState :: TestRunState 'PendingT
pendingState =
    Pending
        (Duration 5)
        (FaultsEnabled True)
        (HasInstrumentation True)
        signature

signature :: Ed25519.Signature
signature =
    case Ed25519.signature (BS.replicate 64 0) of
        CryptoPassed sig -> sig
        CryptoFailed err -> error $ "invalid test signature: " <> show err

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
