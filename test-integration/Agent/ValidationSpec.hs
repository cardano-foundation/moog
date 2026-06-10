{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration coverage of the agent's Antithesis reconcile cycle
-- against the mock Antithesis Warp server ("Lib.Agent.MockAntithesis")
-- and the self-hosted devnet MPFS.
--
--   * the slice-A smoke proves the read seam ('listAllRuns' round-trips
--     the configured runs);
--   * the Accept test proves the Pending→Running transition: a pending
--     test-run fact on-chain plus a matching in-progress Antithesis run
--     drives the real agent Accept, and a Running fact is written.
module Agent.ValidationSpec
    ( agentValidationSpec
    )
where

import Data.Text qualified as T
import Lib.Agent.AcceptHarness
    ( acceptViaAgent
    , factTestRun
    , foldPendingRequests
    , readPendingFacts
    , readRunningFacts
    , sampleTestRun
    , seedConfigAndPending
    , tokenRequestRefIds
    , withAcceptEnv
    )
import Lib.Agent.MockAntithesis (withMockAntithesisServer)
import Oracle.Validate.Types (AValidationResult (..))
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldContain
    , shouldNotContain
    )
import User.Agent.Antithesis.Client
    ( AntithesisApiConfig (..)
    , AntithesisApiKey (..)
    , AntithesisApiUrl (..)
    , listAllRuns
    )
import User.Agent.Antithesis.Plan
    ( PendingAction (..)
    , PollPlan (..)
    , planAgentPoll
    )
import User.Agent.Antithesis.State
    ( AntithesisRun (..)
    , AntithesisRunStatus (..)
    )
import User.Agent.PushTest (renderTestRun)
import User.Agent.Types (mkTestRunId)

agentValidationSpec :: Spec
agentValidationSpec = describe "Agent.Validation" $ do
    it "round-trips the configured runs via the mock Antithesis server" $ do
        result <-
            withMockAntithesisServer (pure sampleRuns) $ \baseUrl ->
                listAllRuns
                    AntithesisApiConfig
                        { antithesisApiUrl = AntithesisApiUrl baseUrl
                        , antithesisApiKey = AntithesisApiKey "api-key"
                        }
        result `shouldBe` Right sampleRuns

    it
        "writes a Running fact when a pending test-run matches an \
        \in-progress Antithesis run"
        $ withAcceptEnv
        $ \env -> do
            let testRun = sampleTestRun
                trId = mkTestRunId testRun
                apiRun =
                    AntithesisRun
                        { antithesisRunId = "run-accept-1"
                        , antithesisRunStatus = RunInProgress
                        , antithesisRunDescription =
                            Just (T.pack (renderTestRun trId testRun))
                        , antithesisRunTriageReport = Nothing
                        }
            -- A pending test-run + its token config seeded on-chain.
            seedConfigAndPending env testRun
            withMockAntithesisServer (pure [apiRun]) $ \baseUrl -> do
                let apiConfig =
                        AntithesisApiConfig
                            { antithesisApiUrl = AntithesisApiUrl baseUrl
                            , antithesisApiKey = AntithesisApiKey "itest-key"
                            }
                apiRuns <-
                    either
                        (\e -> fail $ "listAllRuns: " <> show e)
                        pure
                        =<< listAllRuns apiConfig
                pendingFacts <- readPendingFacts env
                runningBefore <- readRunningFacts env
                -- The real planner pairs the pending fact to the run.
                let plan =
                        planAgentPoll
                            (const True)
                            apiRuns
                            pendingFacts
                            runningBefore
                case pendingActions plan of
                    [PendingAcceptObserved fact run] -> do
                        factTestRun fact `shouldBe` testRun
                        run `shouldBe` apiRun
                    other ->
                        expectationFailure
                            $ "expected one PendingAcceptObserved, got: "
                                <> show other
                -- Drive the agent Accept, signed by the agent wallet.
                acceptRes <- acceptViaAgent env trId
                case acceptRes of
                    ValidationSuccess _ -> pure ()
                    ValidationFailure err ->
                        expectationFailure $ "agent Accept failed: " <> show err
                -- Oracle folds the Accept request into a Running fact.
                acceptRefs <- tokenRequestRefIds env
                foldRes <- foldPendingRequests env acceptRefs
                case foldRes of
                    ValidationSuccess _ -> pure ()
                    ValidationFailure err ->
                        expectationFailure $ "oracle fold failed: " <> show err
                -- The transition is now visible on-chain.
                runningAfter <- readRunningFacts env
                map factTestRun runningAfter `shouldContain` [testRun]
                pendingAfter <- readPendingFacts env
                map factTestRun pendingAfter `shouldNotContain` [testRun]
                -- Accept is idempotent: with no pending fact left, a
                -- second poll neither plans nor writes a transition.
                acceptAgain <- acceptViaAgent env trId
                case acceptAgain of
                    ValidationFailure _ -> pure ()
                    ValidationSuccess _ ->
                        expectationFailure
                            "second Accept unexpectedly succeeded"
                pendingFinal <- readPendingFacts env
                pendingActions
                    (planAgentPoll (const True) apiRuns pendingFinal [])
                    `shouldBe` []
                runningFinal <- readRunningFacts env
                length (filter (== testRun) (map factTestRun runningFinal))
                    `shouldBe` 1

-- | A representative two-run list spanning a non-terminal run with no
-- triage report and a terminal run with one, so the round-trip exercises
-- both the present and absent optional fields.
sampleRuns :: [AntithesisRun]
sampleRuns =
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
        , antithesisRunTriageReport = Just "https://report.example/run-2"
        }
    ]
