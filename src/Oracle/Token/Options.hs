{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Oracle.Token.Options
    ( tokenCommandParser
    , Box (..)
    )
where

import Core.Options
    ( outputReferenceParser
    , tokenIdOption
    , walletOption
    )
import Lib.Box (Box (..))
import OptEnvConf
    ( Alternative (many)
    , Parser
    , command
    , commands
    )
import Oracle.Token.Cli
    ( TokenCommand (..)
    )

tokenCommandParser
    :: Parser (Box TokenCommand)
tokenCommandParser =
    commands
        [ command "update" "Update the token"
            $ fmap (fmap Box) . UpdateToken
                <$> tokenIdOption
                <*> walletOption
                <*> many outputReferenceParser
        , command "boot" "Boot a new token" $ Box . BootToken <$> walletOption
        , command "end" "End the token"
            $ fmap Box . EndToken <$> tokenIdOption <*> walletOption
        ]
