{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module User.Agent.Options
    ( agentCommandParser
    , agentEmailOption
    , agentEmailPasswordOption
    , daysOption
    , testRunIdOption
    , registryOption
    , antithesisAuthOption
    ) where

import Core.Options
    ( downloadAssetsDirectoryOption
    , platformOption
    , repositoryOption
    , tokenIdOption
    , walletOption
    )
import Core.Types.Basic (Duration (..), Success)
import Core.Types.Tx (WithTxHash)
import Lib.Box (Box (..))
import Lib.Options.Secrets (secretsParser)
import OptEnvConf
    ( Alternative (..)
    , Parser
    , auto
    , command
    , commands
    , conf
    , eitherReader
    , env
    , help
    , long
    , metavar
    , option
    , optional
    , reader
    , setting
    , short
    , str
    , strOption
    , value
    )
import Oracle.Validate.DownloadAssets (DownloadAssetsFailure)
import Oracle.Validate.Requests.ManageWhiteList
    ( UpdateWhiteListFailure
    )
import Oracle.Validate.Requests.TestRun.Update (UpdateTestRunFailure)
import Oracle.Validate.Types (AValidationResult)
import User.Agent.Cli
    ( AgentCommand (..)
    , CheckResultsFailure
    , IsReady (NotReady)
    , ReportFailure
    , TestRunId (..)
    )
import User.Agent.PublishResults.Email
    ( EmailPassword (..)
    , EmailUser (..)
    , Result
    )
import User.Agent.PushTest
    ( AntithesisAuth (..)
    , PushFailure
    , Registry (..)
    , SlackWebhook (..)
    )
import User.Agent.Types (TestRunMap)
import User.Types
    ( Phase (..)
    , TestRunRejection (..)
    , TestRunState
    , URL (..)
    )

agentCommandParser
    :: Parser (Box (AgentCommand NotReady))
agentCommandParser =
    commands
        [ command "accept-test" "Request a test on a specific platform"
            $ Box <$> acceptTestOptions
        , command "reject-test" "Reject a test with a reason"
            $ Box <$> rejectTestOptions
        , command "report-test" "Report the result of a test run"
            $ Box <$> reportTestOptions
        , command "query" "Query the status of test runs"
            $ Box <$> queryOptions
        , command "white-list" "Whitelist a repository for test-runs"
            $ Box <$> whitelistRepositoryOptions
        , command "black-list" "Blacklist a repository for test-runs"
            $ Box <$> blacklistRepositoryOptions
        , command "download-assets" "Download test run assets"
            $ Box <$> downloadAssetsOptions
        , command "push-test" "Push a test run to Antithesis"
            $ Box <$> pushTestOptions
        , command "collect-results-for" "Collect test results from email"
            $ Box <$> emailResultsOptions
        , command "collect-all-results" "Collect all test results from email"
            $ Box <$> emailAllResultsOptions
        ]

agentEmailOption :: Parser EmailUser
agentEmailOption =
    EmailUser
        <$> setting
            [ env "ANTI_AGENT_EMAIL"
            , metavar "EMAIL"
            , help "The agent email to access the results mailbox"
            , reader str
            , long "email-user"
            , option
            , conf "agentEmail"
            ]
agentEmailPasswordOption :: Parser EmailPassword
agentEmailPasswordOption =
    EmailPassword
        <$> secretsParser
            "Enter the agent email password to access the mailbox"
            "The agent email password to access the mailbox"
            "ANTI_AGENT_EMAIL_PASSWORD"
            "EMAIL_PASSWORD"
            "ask-agent-email-password"
            "agentEmailPassword"

daysOption :: Parser Int
daysOption =
    setting
        [ long "days"
        , help
            "Number of days in the past to check for test results (default: 7)"
        , metavar "DAYS"
        , reader auto
        , value 7
        , option
        ]

emailResultsOptions
    :: Parser
        (AgentCommand NotReady (AValidationResult CheckResultsFailure Result))
emailResultsOptions =
    CheckResultFor
        <$> tokenIdOption
        <*> agentEmailOption
        <*> agentEmailPasswordOption
        <*> testRunIdOption "check results for"
        <*> daysOption

emailAllResultsOptions
    :: Parser
        (AgentCommand NotReady (AValidationResult CheckResultsFailure [Result]))
emailAllResultsOptions =
    CheckAllResults
        <$> agentEmailOption
        <*> agentEmailPasswordOption
        <*> daysOption

pushTestOptions
    :: Parser
        ( AgentCommand
            NotReady
            (AValidationResult PushFailure Success)
        )
pushTestOptions =
    PushTest
        <$> tokenIdOption
        <*> registryOption
        <*> antithesisAuthOption
        <*> downloadAssetsDirectoryOption
        <*> testRunIdOption "push assets from"
        <*> slackOption

slackOption :: Parser (Maybe SlackWebhook)
slackOption =
    optional
        $ SlackWebhook
            <$> secretsParser
                "Enter the Slack webhook URL for notifications"
                "The Slack webhook URL for notifications"
                "ANTI_SLACK_WEBHOOK"
                "SLACK_WEBHOOK"
                "ask-slack-webhook"
                "slackWebhook"

registryOption :: Parser Registry
registryOption =
    Registry
        <$> strOption
            [ help "URL of the registry where to push the config image"
            , metavar "REGISTRY_URL"
            , long "registry"
            , value
                "us-central1-docker.pkg.dev/molten-verve-216720/cardano-repository"
            ]

antithesisAuthOption :: Parser AntithesisAuth
antithesisAuthOption =
    cardanoWithPwd
        <$> antithesisUserOption
        <*> secretsParser
            "Enter the password to access Antithesis"
            "The password to access Antithesis"
            "ANTI_ANTITHESIS_PASSWORD"
            "PASSWORD"
            "ask-antithesis-password"
            "antithesisPassword"
  where
    cardanoWithPwd username pwd =
        AntithesisAuth{username = username, password = pwd}

antithesisUserOption :: Parser String
antithesisUserOption =
    setting
        [ env "ANTI_ANTITHESIS_USER"
        , metavar "ANTITHESIS_USER"
        , help "The username of the Antithesis account"
        , reader str
        ]

downloadAssetsOptions
    :: Parser
        ( AgentCommand
            NotReady
            ( AValidationResult
                DownloadAssetsFailure
                Success
            )
        )
downloadAssetsOptions =
    DownloadAssets
        <$> tokenIdOption
        <*> testRunIdOption "download assets from"
        <*> downloadAssetsDirectoryOption

whitelistRepositoryOptions
    :: Parser
        ( AgentCommand
            phase
            ( AValidationResult
                UpdateWhiteListFailure
                (WithTxHash Success)
            )
        )
whitelistRepositoryOptions =
    WhiteList
        <$> tokenIdOption
        <*> walletOption
        <*> platformOption
        <*> repositoryOption

blacklistRepositoryOptions
    :: Parser
        ( AgentCommand
            phase
            ( AValidationResult
                UpdateWhiteListFailure
                (WithTxHash Success)
            )
        )
blacklistRepositoryOptions =
    BlackList
        <$> tokenIdOption
        <*> walletOption
        <*> platformOption
        <*> repositoryOption

queryOptions
    :: Parser (AgentCommand NotReady TestRunMap)
queryOptions = Query <$> tokenIdOption

testRunIdOption
    :: String -> Parser TestRunId
testRunIdOption what =
    TestRunId
        <$> setting
            [ long "test-run-id"
            , short 'i'
            , metavar "TEST_RUN_ID"
            , help $ "An ID of a test-run to " ++ what
            , option
            , reader str
            ]

acceptTestOptions
    :: Parser
        ( AgentCommand
            NotReady
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState RunningT))
            )
        )
acceptTestOptions =
    Accept
        <$> tokenIdOption
        <*> walletOption
        <*> testRunIdOption "accept"
        <*> pure ()

testRejectionParser :: Parser TestRunRejection
testRejectionParser =
    setting
        [ long "reason"
        , help "Reason for rejecting the test run"
        , metavar "REJECTION_REASON"
        , reader (eitherReader readReason)
        , option
        , reader (eitherReader readReason)
        ]
  where
    readReason :: String -> Either String TestRunRejection
    readReason "broken-instructions" = Right BrokenInstructions
    readReason "unclear-intent" = Right UnclearIntent
    readReason _ = Left "Unknown reason for rejection"

rejectTestOptions
    :: Parser
        ( AgentCommand
            NotReady
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState DoneT))
            )
        )
rejectTestOptions = do
    testRunId <- testRunIdOption "reject"
    reason <- many testRejectionParser
    tokenId <- tokenIdOption
    wallet <- walletOption
    pure $ Reject tokenId wallet testRunId () reason

reportTestOptions
    :: Parser
        ( AgentCommand
            NotReady
            ( AValidationResult
                ReportFailure
                (WithTxHash (TestRunState DoneT))
            )
        )
reportTestOptions = do
    tokenId <- tokenIdOption
    testRunId <- testRunIdOption "report"
    wallet <- walletOption
    duration <-
        Duration
            <$> setting
                [ long "duration"
                , help "Duration of the test run in hours"
                , metavar "DURATION_HOURS"
                , reader auto
                , option
                ]
    url <-
        URL
            <$> setting
                [ long "url"
                , help "URL of the test report"
                , metavar "REPORT_URL"
                , reader str
                , option
                ]
    pure $ Report tokenId wallet testRunId () duration url
