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
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Core.Options (tokenIdOption, walletOption)
import Core.Types.Basic
    ( Directory (..)
    , Duration
    , Success (..)
    , TokenId
    , Username (..)
    )
import Core.Types.Fact (Fact (..))
import Core.Types.MPFS (MPFSClient (..), mpfsClientOption)
import Core.Types.Tx (WithTxHash)
import Core.Types.Wallet (Wallet)
import Data.Foldable (find, for_)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.QQ (s)
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
import Paths_anti (version)
import System.IO.Temp (withSystemTempDirectory)
import Text.JSON.Canonical
    ( FromJSON (..)
    )
import User.Agent.Cli
    ( AgentCommand (..)
    , ReportFailure
    )
import User.Agent.Lib (testRunDuration)
import User.Agent.Options
    ( agentEmailOption
    , agentEmailPasswordOption
    , antithesisAuthOption
    , daysOption
    , registryOption
    )
import User.Agent.PublishResults.Email
    ( EmailPassword
    , EmailUser
    , Result (..)
    )
import User.Agent.PushTest
    ( AntithesisAuth
    , PushFailure
    , Registry
    )
import User.Agent.Types
    ( TestRunId (..)
    , mkTestRunId
    )
import User.Types (Phase (..), TestRun (..), TestRunState, URL (..))

intro :: String
intro =
    [s|
    Cardano Antithesis Agent Process

    This process will run indefinitely, polling the recipient email for results,
    and changing the relative test-run facts to published when results are found.

    To stop the process, simply interrupt it (Ctrl+C).

    To get help on the available options, use the --help flag.

    To get bash cli completion use

    > source <(anti-agent --bash-completion-script "$(which anti-agent)")

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

data Requesters = Some [Username] | AnyRequester
    deriving (Eq, Show)

allowRequester :: Requesters -> Username -> Bool
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
    , poDays :: Int
    , poTrustedRequesters :: Requesters
    , poRegistry :: Registry
    , poAntithesisAuth :: AntithesisAuth
    , poVerbose :: Bool
    }

processOptionsParser :: Parser ProcessOptions
processOptionsParser =
    ProcessOptions
        <$> githubAuthOption
        <*> pollIntervalOption
        <*> walletOption
        <*> tokenIdOption
        <*> mpfsClientOption
        <*> agentEmailOption
        <*> agentEmailPasswordOption
        <*> daysOption
        <*> requestersOption
        <*> registryOption
        <*> antithesisAuthOption
        <*> verboseOption

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
                $ Username
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
                $ fmap Username
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
        , poVerbose
        , poTrustedRequesters
        } = do
        putStrLn "Starting agent process service..."
        forever $ runExceptT $ do
            results <- liftIO $ pollEmails opts
            loggin
                $ "Found " ++ show (length results) ++ " email results."
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
            for_ results $ \result@Result{description} -> do
                let sameKey :: [Fact TestRun v] -> Maybe (Fact TestRun v)
                    sameKey = find $ (description ==) . factKey
                    matchingTests =
                        Left <$> sameKey runningTests
                            <|> Right <$> sameKey doneTests
                    TestRunId trId = mkTestRunId description
                case matchingTests of
                    Nothing ->
                        loggin
                            $ "No matching test-run found in facts for email result: "
                                ++ trId
                    Just (Left (Fact testRun testState)) -> do
                        loggin
                            $ "Publishing result for test-run: "
                                ++ show testRun
                        eres <- liftIO $ submitDone opts (testRunDuration testState) result
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
                    Just (Right _) -> when poVerbose $ do
                        loggin
                            $ "Test-run "
                                ++ trId
                                ++ " has email result and is already in done state."
            for_ pendingTests $ \(Fact testRun _) -> runExceptT $ do
                let TestRunId trId = mkTestRunId testRun
                    user = requester testRun
                    testId = mkTestRunId testRun
                if allowRequester poTrustedRequesters user
                    then do
                        loggin
                            $ "Test-run "
                                ++ trId
                                ++ " is pending from trusted requester "
                                ++ show user
                                ++ ", starting it."
                        withSystemTempDirectory "antithesis-agent-" $ \directoryPath -> do
                            let directory = Directory directoryPath
                            dres <- liftIO $ downloadAssets opts directory testId
                            case dres of
                                ValidationFailure err -> do
                                    loggin
                                        $ "Failed to download assets for test-run "
                                            ++ trId
                                            ++ ": "
                                            ++ show err
                                    throwE ()
                                ValidationSuccess _ -> pure ()
                            pushes <- liftIO $ pushTest opts directory testId
                            case pushes of
                                ValidationFailure err -> do
                                    loggin
                                        $ "Failed to push test-run "
                                            ++ trId
                                            ++ ": "
                                            ++ show err
                                    throwE ()
                                ValidationSuccess _ -> do
                                    loggin
                                        $ "Pushed test-run "
                                            ++ trId
                                            ++ " to Antithesis."
                        eres <- liftIO $ submitRunning opts testId
                        case eres of
                            ValidationFailure err -> do
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
                    else
                        loggin
                            $ "Test-run "
                                ++ trId
                                ++ " is pending from untrusted requester "
                                ++ show user
                                ++ ", waiting for it to start running."
            for_ runningTests $ \(Fact testRun _) -> do
                let TestRunId trId = mkTestRunId testRun
                    sameKey = (== testRun) . description
                case find sameKey results of
                    Just _ -> pure ()
                    Nothing ->
                        loggin
                            $ "Test-run "
                                ++ trId
                                ++ " is still running, waiting for it to complete."
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

pollEmails :: ProcessOptions -> IO [Result]
pollEmails
    ProcessOptions
        { poMPFSClient
        , poAuth
        , poAntithesisEmail
        , poAntithesisEmailPassword
        , poDays
        } = do
        eresults <-
            cmd
                $ AgentCommand
                    poAuth
                    poMPFSClient
                $ CheckAllResults
                    poAntithesisEmail
                    poAntithesisEmailPassword
                    poDays

        case eresults of
            ValidationFailure err -> error $ "Failed to get email results: " ++ show err
            ValidationSuccess results -> pure results

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
                $ TestRunFacts (AnyTestRuns Nothing [] All)
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
                            (\(Fact tr _) -> tr `notElem` changingState)
                pure
                    $ ValidationSuccess
                        ( notRequested typed
                        , notRequested typed
                        , notRequested typed
                        , changingState
                        )

submitDone
    :: ProcessOptions
    -> Duration
    -> Result
    -> IO
        ( AValidationResult
            ReportFailure
            (WithTxHash (TestRunState DoneT))
        )
submitDone
    ProcessOptions{poAuth, poMPFSClient, poWallet, poTokenId}
    duration
    Result{description, link} =
        cmd
            $ AgentCommand poAuth poMPFSClient
            $ Report
                poTokenId
                poWallet
                (mkTestRunId description)
                ()
                duration
            $ URL
            $ T.unpack link

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
