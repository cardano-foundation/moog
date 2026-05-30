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
            planAgentPoll trusted [] [pendingFact] []
                `shouldBe` PollPlan
                    { pendingActions = [PendingLaunchOnly pendingFact]
                    , runningActions = []
                    }

        it "accepts a pending test only from a matching API observation" $ do
            let observed =
                    run
                        "run-1"
                        RunInProgress
                        (Just matchingDescription)
                        Nothing

            planAgentPoll trusted [observed] [pendingFact] []
                `shouldBe` PollPlan
                    { pendingActions = [PendingAcceptObserved pendingFact observed]
                    , runningActions = []
                    }

        it "does not repost or mutate duplicate pending API matches" $ do
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

            planAgentPoll trusted [runA, runB] [pendingFact] []
                `shouldBe` PollPlan
                    { pendingActions =
                        [PendingSkipDuplicate pendingFact [runA, runB]]
                    , runningActions = []
                    }

        it "finishes a running test from a terminal API run with report URL" $ do
            let observed =
                    run
                        "run-1"
                        RunCompleted
                        (Just matchingDescription)
                        (Just "https://report.example/run-1")

            planAgentPoll trusted [observed] [] [runningFact]
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

        it "does not finish duplicate running API matches" $ do
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

            planAgentPoll trusted [runA, runB] [] [runningFact]
                `shouldBe` PollPlan
                    { pendingActions = []
                    , runningActions =
                        [RunningSkipDuplicate runningFact [runA, runB]]
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
