module Oracle.Validate.Requests.TestRun.Update
    ( validateToDoneCore
    , validateToDoneUpdate
    , validateToRunningCore
    , validateToRunningUpdate
    , AgentRejection (..)
    , UpdateTestRunFailure (..)
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic (Owner)
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Core.Types.Operation (Op (..), Operation (..))
import Data.Foldable (find, for_)
import Effects
    ( Effects (..)
    , KeyFailure
    , updateValidation
    )
import Lib.JSON.Canonical.Extra
import Oracle.Types (requestZooGetTestRunKey)
import Oracle.Validate.Requests.Lib (keyAlreadyPendingFailure)
import Oracle.Validate.Types
    ( ForRole
    , Validate
    , Validated (..)
    , forUser
    , mapFailure
    , notValidated
    )
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))
import User.Types
    ( Phase (..)
    , TestRun (..)
    , TestRunState (..)
    )

data AgentRejection = PreviousStateWrong
    deriving (Show, Eq)

data UpdateTestRunFailure
    = UpdateTestRunKeyFailure KeyFailure
    | UpdateTestRunAgentRejection AgentRejection
    | UpdateTestRunRequestNotFromAgent Owner
    | UpdateTestRunConfigNotAvailable
    | UpdateTestRunPreviousStateNotFound
    | UpdateTestRunTestRunIdNotResolved
    | UpdateTestRunWrongPreviousState
    | UpdateTestRunKeyAlreadyPending TestRun
    deriving (Show, Eq)

instance Monad m => ToJSON m UpdateTestRunFailure where
    toJSON = \case
        UpdateTestRunKeyFailure keyFailure ->
            object ["updateTestRunKeyFailure" .= keyFailure]
        UpdateTestRunAgentRejection rejection ->
            object ["updateTestRunAgentRejection" .= show rejection]
        UpdateTestRunRequestNotFromAgent owner ->
            object ["updateTestRunRequestNotFromAgent" .= show owner]
        UpdateTestRunConfigNotAvailable ->
            toJSON ("Token configuration is not available yet" :: String)
        UpdateTestRunPreviousStateNotFound ->
            toJSON ("Previous state for test run not found" :: String)
        UpdateTestRunTestRunIdNotResolved ->
            toJSON ("Test run ID is not resolved" :: String)
        UpdateTestRunWrongPreviousState ->
            toJSON ("Wrong previous state for test run" :: String)
        UpdateTestRunKeyAlreadyPending testRun ->
            object ["updateTestRunKeyAlreadyPending" .= testRun]

checkingOwner
    :: Monad m
    => Owner
    -> Owner
    -> Validate UpdateTestRunFailure m ()
checkingOwner owner pkh =
    when (owner /= pkh)
        $ notValidated
        $ UpdateTestRunRequestNotFromAgent owner

checkingUpdates
    :: Monad m
    => Operation (OpU x b)
    -> (b -> m (Maybe AgentRejection))
    -> Validate UpdateTestRunFailure m Validated
checkingUpdates operation f = case operation of
    Update _ newState -> do
        result <- lift $ f newState
        for_ result $ notValidated . UpdateTestRunAgentRejection
        pure Validated

validateToDoneUpdate
    :: (Monad m, FromJSON Maybe x)
    => Effects m
    -> ForRole
    -> Owner
    -> Owner
    -> Change TestRun (OpU x (TestRunState DoneT))
    -> Validate UpdateTestRunFailure m Validated
validateToDoneUpdate
    validation
    forRole
    agentPKH
    requester
    change@(Change (Key testRun) operation) = do
        when (forUser forRole)
            $ keyAlreadyPendingFailure
                validation
                UpdateTestRunKeyAlreadyPending
                testRun
                requestZooGetTestRunKey
        mapFailure UpdateTestRunKeyFailure
            $ updateValidation validation change
        checkingOwner requester agentPKH
        checkingUpdates operation
            $ validateToDoneCore validation testRun

validateToDoneCore
    :: Monad m
    => Effects m
    -> TestRun
    -> TestRunState DoneT
    -> m (Maybe AgentRejection)
validateToDoneCore
    validation
    testRun = \case
        Rejected pending _ -> checkPastState validation testRun pending
        Finished accepted _ _ _ -> checkPastState validation testRun accepted

checkPastState
    :: (Monad m, FromJSON Maybe (TestRunState t))
    => Effects m
    -> TestRun
    -> TestRunState t
    -> m (Maybe AgentRejection)
checkPastState Effects{mpfsGetFacts} testRun accepted = do
    testRuns <- mpfsGetFacts
    case find (\(Fact k v _) -> k == testRun && v == accepted) testRuns of
        Just _ -> pure Nothing
        Nothing -> pure $ Just PreviousStateWrong

validateToRunningUpdate
    :: Monad m
    => Effects m
    -> ForRole
    -> Owner
    -> Owner
    -> Change TestRun (OpU (TestRunState PendingT) (TestRunState RunningT))
    -> Validate UpdateTestRunFailure m Validated
validateToRunningUpdate
    validation
    forRole
    agentPKH
    requester
    change@(Change (Key testRun) operation) = do
        when (forUser forRole)
            $ keyAlreadyPendingFailure
                validation
                UpdateTestRunKeyAlreadyPending
                testRun
                requestZooGetTestRunKey
        mapFailure UpdateTestRunKeyFailure
            $ updateValidation validation change
        checkingOwner requester agentPKH
        checkingUpdates operation $ validateToRunningCore validation testRun

validateToRunningCore
    :: Monad m
    => Effects m
    -> TestRun
    -> TestRunState RunningT
    -> m (Maybe AgentRejection)
validateToRunningCore
    validation
    testRun = \case
        Accepted pending -> checkPastState validation testRun pending
