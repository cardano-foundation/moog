{-# LANGUAGE DerivingStrategies #-}

module Wallet.Options
    ( walletCommandParser
    ) where

import Core.Options (walletOption)
import Core.Types.Mnemonics.Options
    ( walletFileArgOption
    , walletFileOption
    , walletPassphraseCommon
    )
import Data.Text (Text)
import Lib.Box (Box (..))
import OptEnvConf (Parser, command, commands, optional)
import Wallet.Cli (WalletCommand (..))

walletCommandParser :: Parser (Box WalletCommand)
walletCommandParser =
    commands
        [ command
            "create"
            "Create a new wallet"
            $ fmap Box . Create
                <$> walletFileOption
                <*> passphraseOption
        , command
            "info"
            "Get the wallet information"
            $ Box . Info
                <$> walletOption
        , command
            "decrypt"
            "Decrypt the wallet's secrets and write it to the specified file"
            $ fmap Box . Decrypt
                <$> walletOption
                <*> walletFileArgOption
        , command
            "encrypt"
            "Encrypt the wallet's secrets and write it to the specified file"
            $ (\w p -> Box . Encrypt w p)
                <$> walletOption
                <*> walletPassphraseCommon
                <*> walletFileArgOption
        ]

passphraseOption :: Parser (Maybe Text)
passphraseOption = optional walletPassphraseCommon
