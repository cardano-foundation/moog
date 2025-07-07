module Oracle.Options
    ( OracleCommand (..)
    , oracleCommandParser
    ) where

import Lib.Box (Box (..), fmapBox)
import Options.Applicative
    ( Parser
    , command
    , helper
    , hsubparser
    , info
    , progDesc
    , (<**>)
    )
import Oracle.Cli (OracleCommand (..))
import Oracle.Token.Options (tokenCommandParser)
import Oracle.Validate.Options (validateCommandParser)

oracleCommandParser :: Parser (Box OracleCommand)
oracleCommandParser =
    hsubparser
        ( command
            "token"
            ( info
                ( fmapBox OracleTokenCommand
                    <$> tokenCommandParser
                    <**> helper
                )
                (progDesc "Manage tokens")
            )
            <> command
                "requests"
                ( info
                    ( Box . OracleValidateCommand
                        <$> validateCommandParser
                        <**> helper
                    )
                    (progDesc "Manage requests")
                )
        )
