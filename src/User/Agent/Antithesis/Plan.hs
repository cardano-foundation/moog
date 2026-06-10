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
    , RunningDecision
        ( RunningFinish
        , RunningFinishDuplicate
        , RunningWaitDuplicate
        )
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
    | -- | Drain duplicate pending matches: accept the canonical run on-chain.
      -- Carries the canonical run and the full duplicate set (logged).
      PendingDrainDuplicate
        (Fact TestRun (TestRunState 'PendingT))
        AntithesisRun
        [AntithesisRun]
    | PendingSkipUntrusted (Fact TestRun (TestRunState 'PendingT))
    deriving (Eq, Show)

data RunningAction
    = RunningWait (Fact TestRun (TestRunState 'RunningT))
    | RunningFinishObserved
        (Fact TestRun (TestRunState 'RunningT))
        AntithesisRun
        Outcome
        URL
    | -- | Drain duplicate running matches whose canonical is terminal:
      -- finish from the canonical's outcome/URL. Carries the full set (logged).
      RunningDrainFinish
        (Fact TestRun (TestRunState 'RunningT))
        AntithesisRun
        Outcome
        URL
        [AntithesisRun]
    | -- | Drain duplicate running matches whose canonical is not yet terminal:
      -- wait. Carries the canonical and the full set (logged).
      RunningDrainWait
        (Fact TestRun (TestRunState 'RunningT))
        AntithesisRun
        [AntithesisRun]
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
                PendingAcceptDuplicate canonical matches ->
                    PendingDrainDuplicate fact canonical matches

    planRunning fact@(Fact testRun _ _) =
        case runningDecision testRun runs of
            State.RunningWait -> RunningWait fact
            RunningFinish run outcome url ->
                RunningFinishObserved fact run outcome url
            RunningFinishDuplicate canonical outcome url matches ->
                RunningDrainFinish fact canonical outcome url matches
            RunningWaitDuplicate canonical matches ->
                RunningDrainWait fact canonical matches
