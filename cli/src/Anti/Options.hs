{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Anti.Options (parseArgs) where

import Anti.Types
    ( Command (..)
    , Directory (..)
    , Host (..)
    , Options (Options)
    , OracleCommand (..)
    , OutputReference (..)
    , Platform (..)
    , Port (..)
    , PublicKeyHash (..)
    , Repository (Repository)
    , Role (..)
    , SHA1 (..)
    , TokenId (..)
    , UserCommand (..)
    , Username (..)
    )
import Options.Applicative
    ( Alternative (..)
    , Parser
    , auto
    , command
    , defaultPrefs
    , execParserPure
    , fullDesc
    , handleParseResult
    , header
    , help
    , helper
    , hsubparser
    , info
    , long
    , maybeReader
    , metavar
    , option
    , progDesc
    , short
    , strOption
    , value
    , (<**>)
    )

platformOption :: Parser Platform
platformOption =
    Platform
        <$> strOption
            ( long "platform"
                <> short 'p'
                <> metavar "PLATFORM"
                <> help "The platform to use"
            )

parseRepository :: String -> Maybe Repository
parseRepository repoStr = case break (== '/') repoStr of
    (org, '/' : proj) -> Just $ Repository org proj
    _ -> Nothing

repositoryOption :: Parser Repository
repositoryOption =
    option
        (maybeReader parseRepository)
        ( long "repository"
            <> short 'r'
            <> metavar "ORGANIZATION/PROJECT"
            <> help "The repository in the format 'organization/project'"
        )

commitOption :: Parser SHA1
commitOption =
    SHA1
        <$> strOption
            ( long "commit"
                <> short 'c'
                <> metavar "COMMIT"
                <> help "The commit hash or reference"
            )

directoryOption :: Parser Directory
directoryOption =
    Directory
        <$> strOption
            ( long "directory"
                <> short 'd'
                <> metavar "DIRECTORY"
                <> value "."
                <> help "The directory to run in (defaults to \".\")"
            )

requestTestOptions :: Parser UserCommand
requestTestOptions =
    RequestTest
        <$> platformOption
        <*> repositoryOption
        <*> commitOption
        <*> directoryOption
        <*> usernameOption

usernameOption :: Parser Username
usernameOption =
    Username
        <$> strOption
            ( long "username"
                <> short 'u'
                <> metavar "USERNAME"
                <> help "The username to register"
            )

pubkeyhashOption :: Parser PublicKeyHash
pubkeyhashOption =
    PublicKeyHash
        <$> strOption
            ( long "pubkeyhash"
                <> short 'k'
                <> metavar "PUBKEYHASH"
                <> help "The public key hash for the user"
            )

addPublicKeyOptions :: Parser UserCommand
addPublicKeyOptions =
    RegisterPublicKey
        <$> platformOption
        <*> usernameOption
        <*> pubkeyhashOption

removePublicKeyOptions :: Parser UserCommand
removePublicKeyOptions =
    UnregisterPublicKey
        <$> platformOption
        <*> usernameOption
        <*> pubkeyhashOption

roleOption :: Parser Role
roleOption =
    Role
        <$> strOption
            ( long "role"
                <> short 'r'
                <> metavar "ROLE"
                <> help "The role to assign to the user (e.g., maintainer, contributor)"
            )

addRoleOptions :: Parser UserCommand
addRoleOptions =
    RegisterRole
        <$> platformOption
        <*> repositoryOption
        <*> roleOption
        <*> usernameOption

removeRoleOptions :: Parser UserCommand
removeRoleOptions =
    UnregisterRole
        <$> platformOption
        <*> repositoryOption
        <*> roleOption
        <*> usernameOption

tokenIdOption :: Parser TokenId
tokenIdOption =
    TokenId
        <$> strOption
            ( long "token-id"
                <> short 't'
                <> metavar "TOKEN_ID"
                <> help "The token ID"
            )

userCommandParser :: Parser UserCommand
userCommandParser =
    hsubparser
        ( command
            "request-test"
            ( info
                requestTestOptions
                (progDesc "Request a test on a specific platform")
            )
            <> command
                "register-public-key"
                ( info
                    addPublicKeyOptions
                    (progDesc "Register a user public key")
                )
            <> command
                "unregister-public-key"
                ( info
                    removePublicKeyOptions
                    (progDesc "Unregister a user public key")
                )
            <> command
                "register-role"
                ( info
                    addRoleOptions
                    (progDesc "Add a user to a repository")
                )
            <> command
                "unregister-role"
                ( info
                    removeRoleOptions
                    (progDesc "Remove a user from a repository")
                )
        )

outputReferenceParser :: Parser OutputReference
outputReferenceParser =
    OutputReference
        <$> strOption
            ( long "tx-hash"
                <> metavar "TX_HASH"
                <> help "The transaction hash for the output reference"
            )
        <*> option
            auto
            ( long "index"
                <> metavar "INDEX"
                <> help "Index of the output reference"
            )

oracleCommandParser :: Parser OracleCommand
oracleCommandParser =
    hsubparser
        ( command
            "create-token"
            ( info
                (pure CreateToken <**> helper)
                (progDesc "Create a new token")
            )
            <> command
                "delete-token"
                ( info
                    (deleteTokenOptions <**> helper)
                    (progDesc "Delete a token")
                )
            <> command
                "get-token"
                ( info
                    (GetToken <$> tokenIdOption <**> helper)
                    (progDesc "Get a token")
                )
            <> command
                "update-token"
                ( info
                    ( UpdateToken
                        <$> tokenIdOption
                        <*> many outputReferenceParser
                        <**> helper
                    )
                    (progDesc "Update a token")
                )
        )

deleteTokenOptions :: Parser OracleCommand
deleteTokenOptions =
    DeleteToken
        <$> tokenIdOption

commandParser :: Parser Command
commandParser =
    hsubparser
        ( command
            "oracle"
            ( info
                (OracleCommand <$> oracleCommandParser <**> helper)
                (progDesc "Manage tokens")
            )
            <> command
                "user"
                ( info
                    ( UserCommand
                        <$> userCommandParser
                        <*> tokenIdOption
                        <**> helper
                    )
                    (progDesc "Manage users")
                )
        )

hostOption :: Parser Host
hostOption =
    Host
        <$> strOption
            ( long "host"
                <> short 'h'
                <> metavar "HOST"
                <> value "localhost"
                <> help "The host to connect to (defaults to \"localhost\")"
            )

portOption :: Parser Port
portOption =
    Port
        <$> option
            auto
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> value 8080
                <> help "The port to connect to (defaults to 8080)"
            )

optionsParser :: Parser Options
optionsParser =
    Options
        <$> hostOption
        <*> portOption
        <*> commandParser

parseArgs :: [String] -> IO Options
parseArgs args = handleParseResult $ execParserPure defaultPrefs opts args
  where
    opts =
        info
            (optionsParser <**> helper)
            ( fullDesc
                <> progDesc "Antithesis CLI"
                <> header "anti - A tool for managing Antithesis test runs"
            )
