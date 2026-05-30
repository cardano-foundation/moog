{-# LANGUAGE OverloadedStrings #-}

-- | Typed Antithesis run observations and pure reconciliation rules.
module User.Agent.Antithesis.State
    ( AntithesisRunStatus (..)
    , AntithesisRun (..)
    , RunsPage (..)
    , PendingDecision (..)
    , RunningDecision (..)
    , parseRunsPage
    , matchingRuns
    , pendingDecision
    , runningDecision
    )
where

import Data.Aeson (FromJSON (..), Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither, withObject, (.:), (.:?))
import Data.Text (Text)
import Data.Text qualified as T
import User.Agent.PushTest (renderTestRun)
import User.Agent.Types (mkTestRunId)
import User.Types
    ( Outcome (..)
    , TestRun
    , URL (..)
    )

data AntithesisRunStatus
    = RunStarting
    | RunInProgress
    | RunCompleted
    | RunCancelled
    | RunIncomplete
    | RunUnknown
    deriving (Eq, Show)

instance FromJSON AntithesisRunStatus where
    parseJSON = Aeson.withText "AntithesisRunStatus" $ \case
        "starting" -> pure RunStarting
        "in_progress" -> pure RunInProgress
        "completed" -> pure RunCompleted
        "cancelled" -> pure RunCancelled
        "incomplete" -> pure RunIncomplete
        "unknown" -> pure RunUnknown
        other -> fail $ "unknown Antithesis run status: " <> T.unpack other

data AntithesisRun = AntithesisRun
    { antithesisRunId :: Text
    , antithesisRunStatus :: AntithesisRunStatus
    , antithesisRunDescription :: Maybe Text
    , antithesisRunTriageReport :: Maybe Text
    }
    deriving (Eq, Show)

instance FromJSON AntithesisRun where
    parseJSON = withObject "AntithesisRun" $ \obj -> do
        antithesisRunId <- obj .: "run_id"
        antithesisRunStatus <- obj .: "status"
        parameters <- obj .:? "parameters"
        links <- obj .:? "links"
        antithesisRunDescription <-
            parseObjectTextField "antithesis.description" parameters
        antithesisRunTriageReport <-
            parseObjectTextField "triage_report" links
        pure AntithesisRun{..}

data RunsPage = RunsPage
    { runsPageRuns :: [AntithesisRun]
    , runsPageNextCursor :: Maybe Text
    }
    deriving (Eq, Show)

instance FromJSON RunsPage where
    parseJSON = withObject "RunsPage" $ \obj ->
        RunsPage
            <$> obj .: "data"
            <*> obj .:? "next_cursor"

data PendingDecision
    = PendingLaunch
    | PendingAccept AntithesisRun
    | PendingDuplicate [AntithesisRun]
    deriving (Eq, Show)

data RunningDecision
    = RunningWait
    | RunningFinish AntithesisRun Outcome URL
    | RunningDuplicate [AntithesisRun]
    deriving (Eq, Show)

parseRunsPage :: Value -> Either String RunsPage
parseRunsPage = parseEither parseJSON

matchingRuns :: TestRun -> [AntithesisRun] -> [AntithesisRun]
matchingRuns testRun =
    filter $
        \run ->
            antithesisRunDescription run
                == Just (T.pack $ renderTestRun (mkTestRunId testRun) testRun)

pendingDecision :: TestRun -> [AntithesisRun] -> PendingDecision
pendingDecision testRun runs =
    case matchingRuns testRun runs of
        [] -> PendingLaunch
        [run] -> PendingAccept run
        matches -> PendingDuplicate matches

runningDecision :: TestRun -> [AntithesisRun] -> RunningDecision
runningDecision testRun runs =
    case matchingRuns testRun runs of
        [] -> RunningWait
        [run] ->
            case antithesisRunTriageReport run of
                Just reportUrl ->
                    case terminalOutcome $ antithesisRunStatus run of
                        Just outcome ->
                            RunningFinish run outcome (URL $ T.unpack reportUrl)
                        Nothing -> RunningWait
                Nothing ->
                    -- Terminal API state with no triage-report link. Finish
                    -- failure-class runs (incomplete, cancelled) on-chain with a
                    -- deterministic synthetic URL so they do not stay accepted
                    -- forever; stay conservative for completed (success is never
                    -- attested without the report link) and for non-terminal.
                    case terminalNoReportOutcome $ antithesisRunStatus run of
                        Just outcome ->
                            RunningFinish run outcome (noTriageReportUrl run)
                        Nothing -> RunningWait
        matches -> RunningDuplicate matches

terminalOutcome :: AntithesisRunStatus -> Maybe Outcome
terminalOutcome = \case
    RunCompleted -> Just OutcomeSuccess
    RunCancelled -> Just OutcomeFailure
    RunIncomplete -> Just OutcomeFailure
    RunStarting -> Nothing
    RunInProgress -> Nothing
    RunUnknown -> Nothing

-- | Outcome for a terminal API run that has no triage-report link.
-- Failure-class terminal states (incomplete, cancelled) are attested as
-- failures so the on-chain run leaves accepted/running; completed stays
-- conservative (success must not be attested without the report link), as do
-- the non-terminal states.
terminalNoReportOutcome :: AntithesisRunStatus -> Maybe Outcome
terminalNoReportOutcome = \case
    RunIncomplete -> Just OutcomeFailure
    RunCancelled -> Just OutcomeFailure
    RunCompleted -> Nothing
    RunStarting -> Nothing
    RunInProgress -> Nothing
    RunUnknown -> Nothing

-- | Deterministic, audit-friendly result URL for a run that reached a terminal
-- API state without a triage report. Depends only on the run id.
noTriageReportUrl :: AntithesisRun -> URL
noTriageReportUrl run =
    URL $
        "antithesis://runs/"
            <> T.unpack (antithesisRunId run)
            <> "/no-triage-report"

parseObjectTextField :: Text -> Maybe Value -> Parser (Maybe Text)
parseObjectTextField _ Nothing = pure Nothing
parseObjectTextField _ (Just Null) = pure Nothing
parseObjectTextField field (Just (Object obj)) =
    case KeyMap.lookup (Key.fromText field) obj of
        Nothing -> pure Nothing
        Just value -> Just <$> parseJSON value
parseObjectTextField field (Just other) =
    fail $
        T.unpack field
            <> " parent must be object or null, got "
            <> show other
