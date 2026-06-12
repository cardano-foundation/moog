{-# LANGUAGE NumericUnderscores #-}
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
import Core.Types.Basic (TokenId (..))
import Lib.Box (Box (..))
import OptEnvConf
    ( Alternative (many)
    , Parser
    , auto
    , command
    , commands
    , env
    , help
    , long
    , metavar
    , option
    , reader
    , setting
    , short
    , str
    , value
    )
import Oracle.Token.Cli
    ( BootParams (..)
    , TokenCommand (..)
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
        , command "reject" "Reject token requests"
            $ fmap (fmap Box) . RejectToken
                <$> rejectTokenIdOption
                <*> walletOption
                <*> many outputReferenceParser
        , command "boot" "Boot a new token"
            $ fmap Box . BootToken
                <$> walletOption
                <*> bootParamsParser
        , command "end" "End the token"
            $ fmap Box . EndToken <$> tokenIdOption <*> walletOption
        ]

rejectTokenIdOption :: Parser TokenId
rejectTokenIdOption =
    TokenId
        <$> setting
            [ env "MOOG_TOKEN_ID"
            , long "token-id"
            , short 't'
            , metavar "TOKEN"
            , help "The token ID of the antithesis token"
            , reader str
            , option
            ]

-- | Operator-supplied boot economics. Defaults are network-safe, not
-- the devnet 5s windows: a request stays processable long enough for
-- the oracle to fold it on a real network (~20s blocks + indexing).
bootParamsParser :: Parser BootParams
bootParamsParser =
    BootParams
        <$> processTimeOption
        <*> retractTimeOption
        <*> tipOption

processTimeOption :: Parser Integer
processTimeOption =
    setting
        [ help
            "Phase-1 process window in ms: the oracle must process a \
            \request within this window of its submission"
        , reader auto
        , long "process-time-ms"
        , metavar "MILLIS"
        , env "MOOG_PROCESS_TIME_MS"
        , option
        , value 180_000
        ]

retractTimeOption :: Parser Integer
retractTimeOption =
    setting
        [ help
            "Phase-2 retract window in ms: the requester may retract \
            \before the oracle may reject"
        , reader auto
        , long "retract-time-ms"
        , metavar "MILLIS"
        , env "MOOG_RETRACT_TIME_MS"
        , option
        , value 180_000
        ]

tipOption :: Parser Integer
tipOption =
    setting
        [ help
            "Oracle tip in lovelace, taken from each request's locked \
            \value when it is processed or rejected"
        , reader auto
        , long "tip"
        , metavar "LOVELACE"
        , env "MOOG_TIP_LOVELACE"
        , option
        , value 1_000_000
        ]
