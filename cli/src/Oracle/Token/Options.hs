{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Oracle.Token.Options
    ( tokenCommandParser
    , Box (..)
    )
where

import Core.Options (outputReferenceParser)
import Lib.Box (Box (..))
import Options.Applicative
    ( Alternative (many)
    , Parser
    , command
    , helper
    , hsubparser
    , info
    , progDesc
    , (<**>)
    )
import Oracle.Token.Cli
    ( TokenCommand (..)
    )

tokenCommandParser :: Parser (Box TokenCommand)
tokenCommandParser =
    hsubparser
        ( command
            "get"
            ( info
                (pure (Box GetToken) <**> helper)
                (progDesc "Get a token")
            )
            <> command
                "update"
                ( info
                    ( Box . UpdateToken
                        <$> many outputReferenceParser
                        <**> helper
                    )
                    (progDesc "Update a token")
                )
            <> command
                "boot"
                ( info
                    (pure (Box BootToken) <**> helper)
                    (progDesc "Boot a token")
                )
            <> command
                "end"
                ( info
                    (pure (Box EndToken) <**> helper)
                    (progDesc "End a token")
                )
        )
