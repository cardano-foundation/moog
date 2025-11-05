module Oracle.Options
    ( OracleCommand (..)
    , oracleCommandParser
    ) where

import Lib.Box (Box (..), fmapBox)
import OptEnvConf
    ( Parser
    , command
    , commands
    )
import Oracle.Cli (OracleCommand (..))
import Oracle.Config.Options (configCommandParser)
import Oracle.Token.Options (tokenCommandParser)

oracleCommandParser
    :: Parser (Box OracleCommand)
oracleCommandParser =
    commands
        [ command "token" "Manage tokens"
            $ fmapBox OracleTokenCommand <$> tokenCommandParser
        , command "config" "Manage configuration"
            $ fmapBox OracleSetConfigCommand <$> configCommandParser
        ]
