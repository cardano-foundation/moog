{-# LANGUAGE OverloadedStrings #-}

-- | Integration coverage of the agent's Antithesis read seam against the
-- mock Antithesis Warp server ("Lib.Agent.MockAntithesis").
--
-- This slice (A) is infrastructure only: a single smoke that boots the
-- mock and asserts 'listAllRuns' round-trips the configured run list,
-- proving the seam works end to end. The Pending→Running and
-- Running→Done reconcile→report transition tests land in slices B\/C.
module Agent.ValidationSpec
    ( agentValidationSpec
    )
where

import Lib.Agent.MockAntithesis (withMockAntithesisServer)
import Test.Hspec (Spec, describe, it, shouldBe)
import User.Agent.Antithesis.Client
    ( AntithesisApiConfig (..)
    , AntithesisApiKey (..)
    , AntithesisApiUrl (..)
    , listAllRuns
    )
import User.Agent.Antithesis.State
    ( AntithesisRun (..)
    , AntithesisRunStatus (..)
    )

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
