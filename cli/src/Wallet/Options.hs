{-# LANGUAGE DerivingStrategies #-}

module Wallet.Options
    ( walletCommandParser
    ) where

import Lib.Box (Box (..))
import Options.Applicative
    ( Parser
    , command
    , hsubparser
    , info
    , progDesc
    )
import Wallet.Cli (WalletCommand (..))

walletCommandParser :: Parser (Box WalletCommand)
walletCommandParser =
    hsubparser
        ( command
            "create"
            ( info
                (pure $ Box Create)
                (progDesc "Create a new wallet")
            )
            <> command
                "info"
                ( info
                    (pure $ Box Info)
                    (progDesc "Get the wallet information")
                )
        )
