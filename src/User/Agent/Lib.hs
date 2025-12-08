module User.Agent.Lib
    ( resolveTestRunId
    , withState
    , testRunDuration
    , testRunFaultsEnabled
    ) where

import Control.Applicative (Alternative (..))
import Core.Context (WithContext, withMPFS)
import Core.Types.Basic (FaultsEnabled, TokenId)
import Core.Types.Duration (Duration)
import Core.Types.Fact (Fact (..), keyHash, parseFacts)
import Data.Foldable (find)
import MPFS.API (MPFS (..))
import Text.JSON.Canonical (FromJSON (..), JSValue)
import User.Agent.Types (TestRunId (..))
import User.Types (Phase (..), TestRun, TestRunState (..))

resolveTestRunId
    :: forall s m
     . (Monad m, FromJSON Maybe s)
    => TokenId
    -> TestRunId
    -> WithContext m (Maybe (Fact TestRun s))
resolveTestRunId tk (TestRunId testRunId) = do
    facts <- fmap parseFacts
        $ withMPFS
        $ \mpfs -> mpfsGetTokenFacts mpfs tk
    let match :: Fact TestRun JSValue -> Bool
        match (Fact key _ _) = case keyHash key of
            Nothing -> False
            Just keyId -> keyId == testRunId
    pure
        $ find match facts >>= \(Fact k v s) ->
            Fact k
                <$> fromJSON v
                <*> pure s

withState
    :: forall a
     . (forall v. TestRunState v -> a)
    -> JSValue
    -> Maybe a
withState f v =
    f <$> state @'PendingT
        <|> f <$> state @'RunningT
        <|> f <$> state @'DoneT
  where
    state
        :: forall s. FromJSON Maybe (TestRunState s) => Maybe (TestRunState s)
    state = fromJSON v

fromPending :: (TestRunState 'PendingT -> a) -> TestRunState v -> a
fromPending f = \case
    p@(Pending{}) -> f p
    (Rejected pending _) -> f pending
    (Accepted pending) -> f pending
    (Finished accepted _ _ _) -> fromPending f accepted

testRunDuration :: TestRunState v -> Duration
testRunDuration = fromPending $ \(Pending d _ _) -> d

testRunFaultsEnabled :: TestRunState v -> FaultsEnabled
testRunFaultsEnabled = fromPending $ \(Pending _ faultsEnabled _) -> faultsEnabled
