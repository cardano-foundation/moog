{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Oracle.Config.Options
    ( configCommandParser
    , Box (..)
    )
where

import Core.Options (tokenIdOption, walletOption)
import Core.Types.Basic (Owner (..))
import Core.Types.Duration (Duration (..))
import Lib.Box (Box (..))
import OptEnvConf
    ( Parser
    , auto
    , command
    , commands
    , help
    , long
    , metavar
    , option
    , reader
    , setting
    , strOption
    )
import Oracle.Config.Cli (ConfigCmd (..))
import Oracle.Config.Types (Config (..))
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )

configOption :: Parser Config
configOption = do
    minDuration <-
        Minutes
            <$> setting
                [ long "min-test-duration"
                , metavar "MINUTES"
                , help "Minimum duration of the tests in minutes"
                , option
                , reader auto
                ]

    maxDuration <-
        Minutes
            <$> setting
                [ long "max-test-duration"
                , metavar "MINUTES"
                , help "Maximum duration of the tests in minutes"
                , option
                , reader auto
                ]
    agent <-
        Owner
            <$> strOption
                [ long "agent-pkh"
                , metavar "PUBLIC_KEY_HASH"
                , help "Public key hash of the agent that will run the tests"
                , option
                ]

    pure
        $ Config
            { configAgent = agent
            , configTestRun =
                TestRunValidationConfig
                    { minDuration
                    , maxDuration
                    }
            }

configCommandParser
    :: Parser (Box ConfigCmd)
configCommandParser =
    commands
        [ command
            "set"
            "Update the oracle configuration"
            ( fmap (fmap Box) . SetConfig
                <$> tokenIdOption
                <*> walletOption
                <*> configOption
            )
        ]
