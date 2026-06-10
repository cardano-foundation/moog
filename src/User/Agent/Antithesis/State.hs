{-# LANGUAGE OverloadedStrings #-}

-- | Typed Antithesis run observations and pure reconciliation rules.
module User.Agent.Antithesis.State
    ( AntithesisRunStatus (..)
    , AntithesisRun (..)
    , RunsPage (..)
    , PendingDecision (..)
    , RunningDecision (..)
    , parseRunsPage
    , descriptionKey
    , matchingRuns
    , canonicalRun
    , pendingDecision
    , runningDecision
    )
where

import Data.Aeson (FromJSON (..), Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither, withObject, (.:), (.:?))
import Data.List (minimumBy)
import Data.Ord (comparing)
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
    | -- | Multiple matching runs: the canonical run plus the full match set.
      PendingAcceptDuplicate AntithesisRun [AntithesisRun]
    deriving (Eq, Show)

data RunningDecision
    = RunningWait
    | RunningFinish AntithesisRun Outcome URL
    | -- | Multiple matching runs whose canonical is terminal-with-result:
      -- the canonical, its outcome/URL, and the full match set.
      RunningFinishDuplicate AntithesisRun Outcome URL [AntithesisRun]
    | -- | Multiple matching runs whose canonical is not yet terminal:
      -- the canonical plus the full match set.
      RunningWaitDuplicate AntithesisRun [AntithesisRun]
    deriving (Eq, Show)

parseRunsPage :: Value -> Either String RunsPage
parseRunsPage = parseEither parseJSON

-- | Deterministic reconciliation key for a test-run: the description
-- Antithesis stores under @antithesis.description@. One stable key per
-- test-run, shared by 'matchingRuns' and (in slice 2) the launch marker.
descriptionKey :: TestRun -> Text
descriptionKey testRun =
    T.pack $ renderTestRun (mkTestRunId testRun) testRun

matchingRuns :: TestRun -> [AntithesisRun] -> [AntithesisRun]
matchingRuns testRun =
    filter $
        \run ->
            antithesisRunDescription run
                == Just (descriptionKey testRun)

-- | Authority ranking of a run's status for the canonical pick. A @completed@
-- run ran to completion and carries the real result, so it outranks an
-- @incomplete@/@cancelled@ duplicate that aborted early, which in turn outrank
-- the non-terminal and @unknown@ states. Higher is more authoritative.
statusPriority :: AntithesisRunStatus -> Int
statusPriority = \case
    RunCompleted -> 4
    RunIncomplete -> 3
    RunCancelled -> 2
    RunInProgress -> 1
    RunStarting -> 1
    RunUnknown -> 0

-- | Pick the canonical run from a set of matching runs by status authority:
-- the highest 'statusPriority' wins, tie-broken by ascending @run_id@ for
-- determinism. This prefers a @completed@ run — which ran to completion and
-- carries the authoritative result — over an @incomplete@/@cancelled@
-- duplicate that aborted early, so a mixed-outcome double-launch finishes from
-- the right run (a prod finding: @min run_id@ alone picked the incomplete run
-- and reported failure). Deterministic and stable across polls so the
-- pending-side and running-side drains always agree on the same run (spec FR6).
-- Total only on non-empty input; only ever called from a non-empty match
-- branch.
canonicalRun :: [AntithesisRun] -> AntithesisRun
canonicalRun =
    minimumBy
        ( comparing $ \r ->
            ( negate (statusPriority (antithesisRunStatus r))
            , antithesisRunId r
            )
        )

-- | The terminal on-chain result of a single matching run, if it has
-- reached one. Covers the report-link case and the #138 no-report case;
-- 'Nothing' when the run is not yet terminal (or is a completed run missing
-- its report link, which stays conservative). Reused for both single and
-- canonical runs so they finish identically.
terminalResult :: AntithesisRun -> Maybe (Outcome, URL)
terminalResult run =
    case antithesisRunTriageReport run of
        Just reportUrl ->
            (,URL $ T.unpack reportUrl)
                <$> terminalOutcome (antithesisRunStatus run)
        Nothing ->
            -- Terminal API state with no triage-report link. Finish
            -- failure-class runs (incomplete, cancelled) on-chain with a
            -- deterministic synthetic URL so they do not stay accepted
            -- forever; stay conservative for completed (success is never
            -- attested without the report link) and for non-terminal.
            (,noTriageReportUrl run)
                <$> terminalNoReportOutcome (antithesisRunStatus run)

pendingDecision :: TestRun -> [AntithesisRun] -> PendingDecision
pendingDecision testRun runs =
    case matchingRuns testRun runs of
        [] -> PendingLaunch
        [run] -> PendingAccept run
        matches -> PendingAcceptDuplicate (canonicalRun matches) matches

runningDecision :: TestRun -> [AntithesisRun] -> RunningDecision
runningDecision testRun runs =
    case matchingRuns testRun runs of
        [] -> RunningWait
        [run] ->
            maybe
                RunningWait
                (uncurry (RunningFinish run))
                (terminalResult run)
        matches ->
            let canonical = canonicalRun matches
            in  maybe
                    (RunningWaitDuplicate canonical matches)
                    ( \(outcome, url) ->
                        RunningFinishDuplicate canonical outcome url matches
                    )
                    (terminalResult canonical)

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
