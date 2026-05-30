{-# LANGUAGE DataKinds #-}

-- | Pure one-poll action planning from on-chain facts plus API observations.
module User.Agent.Antithesis.Plan
    ( PendingAction (..)
    , RunningAction (..)
    , PollPlan (..)
    , planAgentPoll
    )
where

import Core.Types.Basic (GithubUsername)
import Core.Types.Fact (Fact (..))
import qualified User.Agent.Antithesis.State as State
import User.Agent.Antithesis.State
    ( AntithesisRun
    , PendingDecision (..)
    , RunningDecision (RunningDuplicate, RunningFinish)
    , pendingDecision
    , runningDecision
    )
import User.Types
    ( Outcome
    , Phase (..)
    , TestRun
    , TestRunState
    , URL
    , requester
    )

data PendingAction
    = PendingLaunchOnly (Fact TestRun (TestRunState 'PendingT))
    | PendingAcceptObserved (Fact TestRun (TestRunState 'PendingT)) AntithesisRun
    | PendingSkipDuplicate (Fact TestRun (TestRunState 'PendingT)) [AntithesisRun]
    | PendingSkipUntrusted (Fact TestRun (TestRunState 'PendingT))
    deriving (Eq, Show)

data RunningAction
    = RunningWait (Fact TestRun (TestRunState 'RunningT))
    | RunningFinishObserved
        (Fact TestRun (TestRunState 'RunningT))
        AntithesisRun
        Outcome
        URL
    | RunningSkipDuplicate (Fact TestRun (TestRunState 'RunningT)) [AntithesisRun]
    deriving (Eq, Show)

data PollPlan = PollPlan
    { pendingActions :: [PendingAction]
    , runningActions :: [RunningAction]
    }
    deriving (Eq, Show)

planAgentPoll
    :: (GithubUsername -> Bool)
    -> [AntithesisRun]
    -> [Fact TestRun (TestRunState 'PendingT)]
    -> [Fact TestRun (TestRunState 'RunningT)]
    -> PollPlan
planAgentPoll allowRequester runs pendingTests runningTests =
    PollPlan
        { pendingActions = planPending <$> pendingTests
        , runningActions = planRunning <$> runningTests
        }
  where
    planPending fact@(Fact testRun _ _)
        | not $ allowRequester $ requester testRun =
            PendingSkipUntrusted fact
        | otherwise =
            case pendingDecision testRun runs of
                PendingLaunch -> PendingLaunchOnly fact
                PendingAccept run -> PendingAcceptObserved fact run
                PendingDuplicate matches -> PendingSkipDuplicate fact matches

    planRunning fact@(Fact testRun _ _) =
        case runningDecision testRun runs of
            State.RunningWait -> RunningWait fact
            RunningFinish run outcome url ->
                RunningFinishObserved fact run outcome url
            RunningDuplicate matches -> RunningSkipDuplicate fact matches
