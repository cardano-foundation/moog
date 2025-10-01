{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Options
    ( Command (..)
    , commandParser
    , Options (..)
    , optionsParser
    , parseArgs
    , githubAuthOption
    , secretsFileOption
    ) where

import Cli (Command (..))
import Control.Applicative (optional)
import Control.Arrow (left)
import Core.Options
    ( outputReferenceParser
    , tokenIdOption
    , walletOption
    )
import Core.Types.Basic (GithubUsername (..))
import Core.Types.MPFS (mpfsClientOption)
import Data.ByteString.Char8 qualified as B
import Data.Functor (($>))
import Data.String.QQ (s)
import Data.Version (Version)
import Facts (All (..), FactsSelection (..), TestRunSelection (..))
import GitHub (Auth (..))
import Lib.Box (Box (..), fmapBox)
import Lib.Options.Secrets (secretsParser)
import OptEnvConf
    ( Alternative (..)
    , Parser
    , checkEither
    , command
    , commands
    , env
    , help
    , long
    , mapIO
    , metavar
    , option
    , reader
    , runParser
    , setting
    , str
    , switch
    , value
    , withYamlConfig
    , (<|>)
    )
import Oracle.Options (oracleCommandParser)
import Path
    ( Abs
    , File
    , Path
    , SomeBase (Abs, Rel)
    , parseAbsFile
    , parseSomeFile
    , toFilePath
    )
import System.Directory (makeAbsolute)
import User.Agent.Options (agentCommandParser, testRunIdOption)
import User.Agent.Types (TestRunId)
import User.Requester.Options
    ( requesterCommandParser
    , sshClientOption
    , sshClientOptionWithoutSelector
    )
import Wallet.Options (walletCommandParser)

data Options a where
    Options :: Bool -> Command a -> Options a

githubAuthOption :: Parser Auth
githubAuthOption =
    OAuth . B.pack
        <$> secretsParser
            "Enter your GitHub token"
            "The GitHub token"
            "ANTI_GITHUB_PAT"
            "GITHUB_PAT"
            "ask-github-pat"
            "githubPAT"

commandParser :: Parser (Box Command)
commandParser =
    commands
        [ command "oracle" "Manage oracle operations"
            $ (\a c -> fmapBox (OracleCommand a c))
                <$> githubAuthOption
                <*> mpfsClientOption
                <*> oracleCommandParser
        , command "requester" "Manage requester operations"
            $ (\a c -> fmapBox (RequesterCommand a c))
                <$> githubAuthOption
                <*> mpfsClientOption
                <*> requesterCommandParser
        , command "agent" "Manage agent operations"
            $ (\a c -> fmapBox (AgentCommand a c))
                <$> githubAuthOption
                <*> mpfsClientOption
                <*> agentCommandParser
        , command "wallet" "Manage wallet operations"
            $ fmapBox Wallet <$> walletCommandParser
        , command
            "retract"
            "Retract a request"
            retractRequestOptions
        , command "facts" "Get token facts"
            $ (\c tk -> fmapBox (GetFacts c tk))
                <$> mpfsClientOption
                <*> tokenIdOption
                <*> factsSelectionParser
        , command "token" "Get the token content"
            $ (\a -> fmap Box . GetToken a)
                <$> githubAuthOption
                <*> mpfsClientOption
                <*> tokenIdOption
        , command "ssh-selectors" "List key selectors for an SSH key file"
            $ Box . SSHSelectors <$> sshClientOptionWithoutSelector
        ]

factsSelectionParser :: Parser (Box FactsSelection)
factsSelectionParser =
    commands
        [ command "users" "Get registered users" (pure $ Box UserFacts)
        , command "roles" "Get registered roles" (pure $ Box RoleFacts)
        , command
            "test-runs"
            "Get test runs"
            (fmapBox TestRunFacts <$> testRunSelectionParser)
        , command
            "config"
            "Get the oracle configuration"
            (pure $ Box ConfigFact)
        , command
            "white-list"
            "Get white-listed repositories"
            (pure $ Box WhiteListedFacts)
        ]
        <|> pure (Box AllFacts)

testRunSelectionParser :: Parser (Box TestRunSelection)
testRunSelectionParser =
    commands
        [ command
            "pending"
            "Get pending test runs"
            (fmap Box $ TestRunPending <$> includedTestRuns <*> whoseOption)
        , command
            "running"
            "Get running test runs"
            (fmap Box $ TestRunRunning <$> includedTestRuns <*> whoseOption)
        , command
            "done"
            "Get done test runs"
            ( fmap Box
                $ TestRunDone
                    <$> optional walletOption
                    <*> optional sshClientOption
                    <*> includedTestRuns
                    <*> whoseOption
            )
        , command
            "rejected"
            "Get rejected test runs"
            (fmap Box $ TestRunRejected <$> includedTestRuns <*> whoseOption)
        ]
        <|> fmap
            Box
            ( AnyTestRuns
                <$> optional walletOption
                <*> optional sshClientOption
                <*> includedTestRuns
                <*> whoseOption
            )

whoseOption :: Parser All
whoseOption =
    setting
        [ help "Filter test runs by requester username"
        , metavar "USERNAME|ALL"
        , long "whose"
        , option
        , reader $ Requester . GithubUsername <$> str
        , value All
        ]

includedTestRuns :: Parser [TestRunId]
includedTestRuns = many $ testRunIdOption "include"

prettyOption :: Parser Bool
prettyOption =
    ((&&) . not <$> noPrettyOption)
        <*> setting
            [ env "ANTI_PRETTY"
            , help "Pretty print JSON output"
            , metavar "NONE"
            , long "pretty"
            , switch True
            , value False
            , reader (str @String $> True)
            ]

noPrettyOption :: Parser Bool
noPrettyOption =
    setting
        [ help
            "Do not pretty print JSON output (overrides --pretty / ANTI_PRETTY)"
        , long "no-pretty"
        , switch True
        , value False
        , reader (str @String $> False)
        ]

optionsParser :: Parser (Box Options)
optionsParser = (\c -> fmapBox $ Options c) <$> prettyOption <*> commandParser

intro :: String
intro =
    [s|
    anti - A tool for managing Antithesis test runs on Cardano

    This tool is used by all 3 roles in the Cardano to Antithesis interface.
    The roles are:
      - Requester: Requests test runs on Antithesis
      - Agent: Manage test runs on Antithesis
      - Oracle: Manages the interface state database (see oracle-anti for an automated oracle process)

    Each role has a set of commands that can be used to manage their respective
    operations. Use the --help flag to see the available commands and options.

    To get bash cli completion use

    > source <(anti --bash-completion-script "$(which anti)")

    Fish and zsh completions are also available.
    |]

parseArgs :: Version -> IO (Box Options)
parseArgs version =
    runParser
        version
        intro
        $ withYamlConfig secretsFileOption optionsParser

retractRequestOptions :: Parser (Box Command)
retractRequestOptions =
    fmap (fmap Box) . RetractRequest
        <$> mpfsClientOption
        <*> walletOption
        <*> outputReferenceParser

secretsFileOption :: Parser (Maybe (Path Abs File))
secretsFileOption =
    optional
        $ parsePath
        $ setting
            [ long "secrets-file"
            , metavar "FILEPATH"
            , help "The file path to a YAML file containing secrets"
            , reader str
            , option
            , env "ANTI_SECRETS_FILE"
            ]
  where
    parsePath = mapIO absolutize . checkEither (left show . parseSomeFile)
    absolutize (Abs fp) = pure fp
    absolutize (Rel fp) = makeAbsolute (toFilePath fp) >>= parseAbsFile
