module Options
    ( Command (..)
    , commandParser
    , Options (..)
    , optionsParser
    , parseArgs
    ) where

import Cli (Command (..))
import Core.Options (outputReferenceParser)
import Lib.Box (Box (..), fmapBox)
import Options.Applicative
    ( Parser
    , command
    , defaultPrefs
    , execParserPure
    , fullDesc
    , handleParseResult
    , header
    , helper
    , hsubparser
    , info
    , progDesc
    , (<**>)
    )
import Oracle.Options (oracleCommandParser)
import User.Agent.Options (agentCommandParser)
import User.Requester.Options (requesterCommandParser)
import Wallet.Options (walletCommandParser)

newtype Options a = Options
    { optionsCommand :: Command a
    }
    deriving (Eq, Show)

commandParser :: Parser (Box Command)
commandParser =
    hsubparser
        ( command
            "oracle"
            ( info
                (fmapBox OracleCommand <$> oracleCommandParser)
                (progDesc "Manage token updates")
            )
            <> command
                "requester"
                ( info
                    (fmapBox RequesterCommand <$> requesterCommandParser)
                    (progDesc "Manage requester changes")
                )
            <> command
                "retract"
                ( info
                    retractRequestOptions
                    (progDesc "Retract a change")
                )
            <> command
                "facts"
                ( info
                    (pure . Box $ GetFacts)
                    (progDesc "Get token facts")
                )
            <> command
                "agent"
                ( info
                    (fmapBox AgentCommand <$> agentCommandParser)
                    (progDesc "Manage agent changes")
                )
            <> command
                "wallet"
                ( info
                    (fmapBox Wallet <$> walletCommandParser)
                    (progDesc "Manage wallet operations")
                )
        )

optionsParser :: Parser (Box Options)
optionsParser = fmapBox Options <$> commandParser

parseArgs :: [String] -> IO (Box Options)
parseArgs args = handleParseResult $ execParserPure defaultPrefs opts args
  where
    opts =
        info
            (optionsParser <**> helper)
            ( fullDesc
                <> progDesc "Antithesis CLI"
                <> header "anti - A tool for managing Antithesis test runs"
            )

retractRequestOptions :: Parser (Box Command)
retractRequestOptions =
    Box . RetractRequest
        <$> outputReferenceParser
