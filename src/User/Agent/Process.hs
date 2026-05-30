{-
This is a full automated agent process that can be used in the CLI.
The process
- monitors the antithesis recipients email inbox for new test results.
- monitors the running test-run facts
- publish a report-test transaction for each new result found.
- automatically accepts pending test-runs from trusted requesters, downloading their assets and pushing them to Antithesis.

Trickery:
- To avoid pushing to Antithesis twice the same test-run, we have to track also the test-runs that are changing state (pending->running or running->done).
  This is done by checking the token requests and excluding from the pending/running/done lists any test-run that is currently changing state.
-}
{-# LANGUAGE QuasiQuotes #-}

module User.Agent.Process
    ( ProcessOptions (..)
    , agentProcess
    , parseArgs
    ) where

import Cli (Command (..), TokenInfoFailure, WithValidation (..), cmd)
import Control.Applicative (Alternative (..), optional)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Core.Options (tokenIdOption, walletOption)
import Core.Types.Basic
    ( Directory (..)
    , Duration
    , GithubUsername (..)
    , Success (..)
    , TokenId
    )
import Core.Types.Fact (Fact (..))
import Core.Types.MPFS (MPFSClient (..), mpfsClientOption)
import Core.Types.Tx (WithTxHash)
import Core.Types.Wallet (Wallet)
import Data.CaseInsensitive (mk)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.QQ (s)
import Data.List (intercalate)
import Data.Text qualified as T
import Facts (All (..), FactsSelection (..), TestRunSelection (..))
import GitHub (Auth)
import OptEnvConf
    ( Parser
    , conf
    , help
    , long
    , metavar
    , runParser
    , setting
    , short
    , strOption
    , switch
    , value
    , withYamlConfig
    )
import Options (githubAuthOption, secretsFileOption)
import Oracle.Process (pollIntervalOption)
import Oracle.Types (Token (..), requestZooGetTestRunKey)
import Oracle.Validate.DownloadAssets (DownloadAssetsFailure)
import Oracle.Validate.Requests.TestRun.Update (UpdateTestRunFailure)
import Oracle.Validate.Types (AValidationResult (..))
import Paths_moog (version)
import System.IO.Temp (withSystemTempDirectory)
import Text.JSON.Canonical
    ( FromJSON (..)
    )
import User.Agent.Cli
    ( AgentCommand (..)
    , ReportFailure
    )
import User.Agent.Antithesis.Client
    ( AntithesisApiConfig (..)
    , AntithesisApiKey
    , AntithesisApiUrl
    , deriveAntithesisApiUrl
    , listAllRuns
    )
import User.Agent.Antithesis.Plan
    ( PendingAction (..)
    , PollPlan (..)
    , RunningAction (..)
    , planAgentPoll
    )
import User.Agent.Antithesis.State
    ( AntithesisRun (..)
    )
import User.Agent.Lib (testRunDuration)
import User.Agent.Options
    ( agentEmailOption
    , agentEmailPasswordOption
    , antithesisApiKeyOption
    , antithesisApiUrlOption
    , antithesisAuthOption
    , minutesOption
    , registryOption
    )
import User.Agent.PublishResults.Email
    ( EmailPassword
    , EmailReadSummary (..)
    , EmailUser
    , Minutes
    , Result (..)
    , readEmailsWithSummary
    )
import User.Agent.PushTest
    ( AntithesisAuth (..)
    , PushFailure
    , Registry
    )
import User.Agent.Types
    ( TestRunId (..)
    , mkTestRunId
    )
import User.Types (Outcome, Phase (..), TestRun (..), TestRunState, URL (..))

intro :: String
intro =
    [s|
    Cardano Antithesis Agent Process

    This process will run indefinitely, polling Antithesis API state and
    test-run facts, then mirroring API-observed accepted and finished states
    on-chain.

    To stop the process, simply interrupt it (Ctrl+C).

    To get help on the available options, use the --help flag.

    To get bash cli completion use

    > source <(moog-agent --bash-completion-script "$(which moog-agent)")

    Fish and zsh completions are also available.
    |]

parseArgs :: IO ProcessOptions
parseArgs =
    runParser
        version
        intro
        $ withYamlConfig
            secretsFileOption
            processOptionsParser

data Requesters = Some [GithubUsername] | AnyRequester
    deriving (Eq, Show)

allowRequester :: Requesters -> GithubUsername -> Bool
allowRequester AnyRequester _ = True
allowRequester (Some users) user = user `elem` users

data ProcessOptions = ProcessOptions
    { poAuth :: Auth
    , poPollIntervalSeconds :: Int
    , poWallet :: Wallet
    , poTokenId :: TokenId
    , poMPFSClient :: MPFSClient
    , poAntithesisEmail :: EmailUser
    , poAntithesisEmailPassword :: EmailPassword
    , poMinutes :: Minutes
    , poTrustedRequesters :: Requesters
    , poRegistry :: Registry
    , poAntithesisAuth :: AntithesisAuth
    , poAntithesisApiConfig :: AntithesisApiConfig
    , poVerbose :: Bool
    }

data EmailPoll = EmailPoll
    { emailPollResults :: [Result]
    , emailPollSummary :: EmailReadSummary
    }

processOptionsParser :: Parser ProcessOptions
processOptionsParser =
    mkProcessOptions
        <$> githubAuthOption
        <*> pollIntervalOption
        <*> walletOption
        <*> tokenIdOption
        <*> mpfsClientOption
        <*> agentEmailOption
        <*> agentEmailPasswordOption
        <*> minutesOption
        <*> requestersOption
        <*> registryOption
        <*> antithesisAuthOption
        <*> antithesisApiKeyOption
        <*> antithesisApiUrlOption
        <*> verboseOption

mkProcessOptions
    :: Auth
    -> Int
    -> Wallet
    -> TokenId
    -> MPFSClient
    -> EmailUser
    -> EmailPassword
    -> Minutes
    -> Requesters
    -> Registry
    -> AntithesisAuth
    -> AntithesisApiKey
    -> Maybe AntithesisApiUrl
    -> Bool
    -> ProcessOptions
mkProcessOptions
    poAuth
    poPollIntervalSeconds
    poWallet
    poTokenId
    poMPFSClient
    poAntithesisEmail
    poAntithesisEmailPassword
    poMinutes
    poTrustedRequesters
    poRegistry
    poAntithesisAuth
    apiKey
    maybeApiUrl
    poVerbose =
        ProcessOptions
            { poAuth
            , poPollIntervalSeconds
            , poWallet
            , poTokenId
            , poMPFSClient
            , poAntithesisEmail
            , poAntithesisEmailPassword
            , poMinutes
            , poTrustedRequesters
            , poRegistry
            , poAntithesisAuth
            , poAntithesisApiConfig =
                AntithesisApiConfig
                    { antithesisApiUrl =
                        fromMaybe
                            (deriveAntithesisApiUrl $ launchUrl poAntithesisAuth)
                            maybeApiUrl
                    , antithesisApiKey = apiKey
                    }
            , poVerbose
            }

verboseOption :: Parser Bool
verboseOption =
    setting
        [ long "verbose"
        , help "Enable verbose logging."
        , metavar "VERBOSE"
        , switch True
        , value False
        ]

someRequestersOption :: Parser Requesters
someRequestersOption =
    let fromOption =
            many
                $ GithubUsername . mk
                    <$> strOption
                        [ long "trusted-test-requester"
                        , short 'c'
                        , metavar "GITHUB_USERNAME"
                        , help
                            "GitHub username of a trusted test-run requester. \
                            \Can be specified multiple times to add multiple trusted requesters. \
                            \All test-runs pending from trusted requesters will be run by the agent."
                        ]
        fromConfig =
            fmap (fromMaybe [])
                $ optional
                $ fmap (GithubUsername . mk)
                    <$> setting
                        [ conf "trustedRequesters"
                        , help "List of trusted test-run requesters GitHub usernames."
                        , metavar "GITHUB_USERNAMES"
                        ]
    in  fmap Some $ (<>) <$> fromOption <*> fromConfig

allRequestersOption :: Parser Requesters
allRequestersOption =
    setting
        [ long "trust-all-requesters"
        , help
            "Trust all test-run requesters. All pending test-runs will be run by the agent."
        , switch AnyRequester
        ]

requestersOption :: Parser Requesters
requestersOption = allRequestersOption <|> someRequestersOption

agentProcess
    :: ProcessOptions
    -> IO ()
agentProcess
    opts@ProcessOptions
        { poPollIntervalSeconds
        , poTrustedRequesters
        , poAntithesisApiConfig
        } = do
        putStrLn "Starting agent process service..."
        forever $ runExceptT $ do
            apiRuns <-
                liftIO (listAllRuns poAntithesisApiConfig) >>= \case
                    Left err -> do
                        loggin $
                            "Failed to get Antithesis API runs: "
                                ++ show err
                        pure Nothing
                    Right runs -> do
                        loggin $
                            "Found "
                                ++ show (length runs)
                                ++ " Antithesis API runs."
                        pure $ Just runs
            efacts <- liftIO $ pollTestRuns opts
            (pendingTests, runningTests, doneTests, stateChanging) <- case efacts of
                ValidationFailure err -> do
                    loggin $ "Failed to get test-run facts: " ++ show err
                    throwE ()
                ValidationSuccess facts -> pure facts

            loggin
                $ "Found "
                    ++ show (length pendingTests)
                    ++ " pending tests (excluding changing state), "
                    ++ show (length runningTests)
                    ++ " running tests (excluding changing state), and "
                    ++ show (length doneTests)
                    ++ " done tests (excluding changing state). and "
                    ++ show (length stateChanging)
                    ++ " changing state tests."
            case apiRuns of
                Nothing ->
                    loggin
                        "Skipping pending/running transitions because Antithesis API state is unavailable."
                Just runs -> do
                    let PollPlan{pendingActions, runningActions} =
                            planAgentPoll
                                (allowRequester poTrustedRequesters)
                                runs
                                pendingTests
                                runningTests
                    for_ pendingActions $ liftIO . executePendingAction opts
                    for_ runningActions $ liftIO . executeRunningAction opts
            for_ stateChanging $ \testRun -> do
                let TestRunId trId = mkTestRunId testRun
                loggin
                    $ "Test-run "
                        ++ trId
                        ++ " is changing state (pending->running or running->done), waiting for it to settle."
            loggin
                $ "Sleeping for "
                    ++ show poPollIntervalSeconds
                    ++ " seconds..."
            liftIO $ threadDelay (poPollIntervalSeconds * 1000000)

loggin :: MonadIO m => String -> m ()
loggin = liftIO . putStrLn

executePendingAction :: ProcessOptions -> PendingAction -> IO ()
executePendingAction opts = \case
    PendingLaunchOnly fact@(Fact testRun _ _) -> do
        let TestRunId trId = mkTestRunId testRun
            user = requester testRun
        loggin
            $ "Test-run "
                ++ trId
                ++ " is pending from trusted requester "
                ++ show user
                ++ ", launching it without accepting on-chain in this poll."
        launchPendingTest opts fact
    PendingAcceptObserved (Fact testRun _ _) run -> do
        let testId@(TestRunId trId) = mkTestRunId testRun
        loggin
            $ "Accepting test-run "
                ++ trId
                ++ " from Antithesis API run "
                ++ T.unpack (antithesisRunId run)
                ++ "."
        eres <- submitRunning opts testId
        case eres of
            ValidationFailure err ->
                loggin
                    $ "Failed to accept test-run "
                        ++ trId
                        ++ ": "
                        ++ show err
            ValidationSuccess txHash ->
                loggin
                    $ "Accepted test-run "
                        ++ trId
                        ++ " in transaction "
                        ++ show txHash
    PendingSkipDuplicate (Fact testRun _ _) runs -> do
        let TestRunId trId = mkTestRunId testRun
        loggin
            $ "Test-run "
                ++ trId
                ++ " has duplicate Antithesis API runs "
                ++ runIds runs
                ++ "; skipping launch and on-chain transition."
    PendingSkipUntrusted (Fact testRun _ _) ->
        loggin
            $ "Test-run "
                ++ trId
                ++ " is pending from untrusted requester "
                ++ show user
                ++ ", waiting for it to start running."
      where
        TestRunId trId = mkTestRunId testRun
        user = requester testRun

executeRunningAction :: ProcessOptions -> RunningAction -> IO ()
executeRunningAction opts = \case
    RunningWait (Fact testRun _ _) -> do
        let TestRunId trId = mkTestRunId testRun
        loggin
            $ "Test-run "
                ++ trId
                ++ " is still running according to Antithesis API."
    RunningFinishObserved (Fact testRun testState _) run outcome url -> do
        let testId@(TestRunId trId) = mkTestRunId testRun
            URL urlStr = url
        loggin
            $ "Publishing result for test-run "
                ++ trId
                ++ " from Antithesis API run "
                ++ T.unpack (antithesisRunId run)
                ++ " with result URL "
                ++ urlStr
                ++ "."
        eres <- submitDone opts testId (testRunDuration testState) outcome url
        case eres of
            ValidationFailure err ->
                loggin
                    $ "Failed to publish result for test-run "
                        ++ trId
                        ++ ": "
                        ++ show err
            ValidationSuccess txHash ->
                loggin
                    $ "Published result for test-run "
                        ++ trId
                        ++ " in transaction "
                        ++ show txHash
    RunningSkipDuplicate (Fact testRun _ _) runs -> do
        let TestRunId trId = mkTestRunId testRun
        loggin
            $ "Test-run "
                ++ trId
                ++ " has duplicate Antithesis API runs "
                ++ runIds runs
                ++ "; skipping finished transition."

launchPendingTest
    :: ProcessOptions
    -> Fact TestRun (TestRunState 'PendingT)
    -> IO ()
launchPendingTest opts (Fact testRun _ _) = do
    let testId@(TestRunId trId) = mkTestRunId testRun
    withSystemTempDirectory "moog-agent-" $ \directoryPath -> do
        let directory = Directory directoryPath
        dres <- downloadAssets opts directory testId
        case dres of
            ValidationFailure err ->
                loggin
                    $ "Failed to download assets for test-run "
                        ++ trId
                        ++ ": "
                        ++ show err
            ValidationSuccess _ -> do
                pushes <- pushTest opts directory testId
                case pushes of
                    ValidationFailure err ->
                        loggin
                            $ "Failed to push test-run "
                                ++ trId
                                ++ ": "
                                ++ show err
                    ValidationSuccess _ ->
                        loggin
                            $ "Pushed test-run "
                                ++ trId
                                ++ " to Antithesis; waiting for a later API observation before accepting on-chain."

runIds :: [AntithesisRun] -> String
runIds =
    intercalate ", " . fmap (T.unpack . antithesisRunId)

emailPollSummaryMessage :: EmailReadSummary -> String
emailPollSummaryMessage
    EmailReadSummary
        { emailCandidateCount
        , emailWithinWindowCount
        , emailParsedCount
        , emailRejectedCount
        , emailDroppedByWindowCount
        } =
        "Found "
            ++ show emailParsedCount
            ++ " email results from "
            ++ show emailCandidateCount
            ++ " Antithesis candidate emails ("
            ++ show emailWithinWindowCount
            ++ " inside recovery window, "
            ++ show emailRejectedCount
            ++ " parse failures, "
            ++ show emailDroppedByWindowCount
            ++ " outside recovery window)."

pollEmails :: ProcessOptions -> IO EmailPoll
pollEmails
    ProcessOptions
        { poAntithesisEmail
        , poAntithesisEmailPassword
        , poMinutes
        } = do
        eresults <-
            runExceptT
                $ readEmailsWithSummary
                    poAntithesisEmail
                    poAntithesisEmailPassword
                    poMinutes

        case eresults of
            Left err -> error $ "Failed to get email results: " ++ show err
            Right (resultsOrErrors, emailPollSummary) -> do
                let (_, emailPollResults) = partitionEithers resultsOrErrors
                pure EmailPoll{emailPollResults, emailPollSummary}

pollTestRuns
    :: ProcessOptions
    -> IO
        ( AValidationResult
            TokenInfoFailure
            ( [Fact TestRun (TestRunState 'PendingT)]
            , [Fact TestRun (TestRunState 'RunningT)]
            , [Fact TestRun (TestRunState 'DoneT)]
            , [TestRun]
            )
        )
pollTestRuns
    ProcessOptions
        { poMPFSClient
        , poTokenId
        , poAuth
        } = do
        allTrs <-
            cmd
                $ GetFacts poMPFSClient poTokenId
                $ TestRunFacts (AnyTestRuns Nothing Nothing [] All)
        let typed :: FromJSON Maybe x => [Fact TestRun x]
            typed = mapMaybe (mapM fromJSON) allTrs
        etoken <- cmd $ GetToken poAuth poMPFSClient poTokenId
        case etoken of
            ValidationFailure err -> pure $ ValidationFailure err
            ValidationSuccess token -> do
                let changingState =
                        mapMaybe
                            (requestZooGetTestRunKey . request)
                            (tokenRequests token)
                    notRequested :: [Fact TestRun v] -> [Fact TestRun v]
                    notRequested =
                        filter
                            (\(Fact tr _ _) -> tr `notElem` changingState)
                pure
                    $ ValidationSuccess
                        ( notRequested typed
                        , notRequested typed
                        , notRequested typed
                        , changingState
                        )

submitDone
    :: ProcessOptions
    -> TestRunId
    -> Duration
    -> Outcome
    -> URL
    -> IO
        ( AValidationResult
            ReportFailure
            (WithTxHash (TestRunState DoneT))
        )
submitDone
    ProcessOptions{poAuth, poMPFSClient, poWallet, poTokenId}
    testId
    duration
    outcome
    url =
        cmd
            $ AgentCommand poAuth poMPFSClient
            $ Report
                poTokenId
                poWallet
                testId
                ()
                duration
                outcome
            $ url

submitRunning
    :: ProcessOptions
    -> TestRunId
    -> IO
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState 'RunningT))
        )
submitRunning
    ProcessOptions{poAuth, poMPFSClient, poWallet, poTokenId}
    testId =
        cmd
            $ AgentCommand poAuth poMPFSClient
            $ Accept
                poTokenId
                poWallet
                testId
                ()

downloadAssets
    :: ProcessOptions
    -> Directory
    -> TestRunId
    -> IO (AValidationResult DownloadAssetsFailure Success)
downloadAssets
    ProcessOptions{poAuth, poMPFSClient, poTokenId}
    directory
    testId =
        cmd
            $ AgentCommand poAuth poMPFSClient
            $ DownloadAssets
                poTokenId
                testId
                directory
pushTest
    :: ProcessOptions
    -> Directory
    -> TestRunId
    -> IO
        ( AValidationResult
            PushFailure
            Success
        )
pushTest
    ProcessOptions
        { poAuth
        , poMPFSClient
        , poTokenId
        , poRegistry
        , poAntithesisAuth
        }
    directory
    testId =
        cmd
            $ AgentCommand poAuth poMPFSClient
            $ PushTest
                poTokenId
                poRegistry
                poAntithesisAuth
                directory
                testId
                Nothing
