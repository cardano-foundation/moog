{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Pop.Cli where

import Data.Aeson (FromJSON, ToJSON, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import Network.HTTP.Simple (
    Request,
    Response,
    getResponseBody,
    getResponseStatusCode,
    httpLBS,
    parseRequest,
    setRequestBodyLBS,
    setRequestMethod,
 )
import Options.Applicative (
    Parser,
    command,
    defaultPrefs,
    execParserPure,
    fullDesc,
    handleParseResult,
    header,
    help,
    helper,
    info,
    long,
    maybeReader,
    metavar,
    option,
    progDesc,
    short,
    strOption,
    subparser,
    value,
    (<**>),
 )

type Args = [String]

type Platform = String

type SHA1 = String

type TxId = String

data Repository = Repository
    { organization :: String
    , project :: String
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Command
    = Request {platform :: Platform, repository :: Repository, commit :: SHA1, directory :: String}
    | Register {platform :: Platform, username :: String, pubkeyhash :: String}
    | AddUser {platform :: Platform, repository :: Repository, role :: String, userIdentifier :: String}
    | RemoveUser {platform :: Platform, repository :: Repository, userIdentifier :: String}

platformOption :: Parser Platform
platformOption =
    strOption
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

requestOptions :: Parser Command
requestOptions =
    Request
        <$> platformOption
        <*> repositoryOption
        <*> strOption
            ( long "commit"
                <> short 'c'
                <> metavar "COMMIT"
                <> help "The commit hash or reference"
            )
        <*> strOption
            ( long "directory"
                <> short 'd'
                <> metavar "DIRECTORY"
                <> value "."
                <> help "Directory to run in (defaults to \".\")"
            )

registerOptions :: Parser Command
registerOptions =
    Register
        <$> platformOption
        <*> strOption
            ( long "username"
                <> short 'u'
                <> metavar "USERNAME"
                <> help "The username to register"
            )
        <*> strOption
            ( long "pubkeyhash"
                <> metavar "PUBKEYHASH"
                <> help "The public key hash for the user"
            )

addUserOptions :: Parser Command
addUserOptions =
    AddUser
        <$> platformOption
        <*> repositoryOption
        <*> strOption
            ( long "role"
                <> metavar "ROLE"
                <> help "The role to assign to the user (e.g., maintainer, contributor)"
            )
        <*> strOption
            ( long "user-id"
                <> metavar "USER-ID"
                <> help "The ID of the user to add, given as '<repository>/<username>'"
            )

removeUserOptions :: Parser Command
removeUserOptions =
    RemoveUser
        <$> platformOption
        <*> repositoryOption
        <*> strOption
            ( long "user-id"
                <> metavar "USER-ID"
                <> help "The ID of the user to remove, given as '<repository>/<username>'"
            )

commandParser :: Parser Command
commandParser =
    subparser
        ( command "request" (info requestOptions (progDesc "Request a test on a specific platform"))
            <> command "register" (info registerOptions (progDesc "Register a new user"))
            <> command "add-user" (info addUserOptions (progDesc "Add a user to a repository"))
            <> command "remove-user" (info removeUserOptions (progDesc "Remove a user from a repository"))
        )

parseArgs :: [String] -> IO Command
parseArgs args = handleParseResult $ execParserPure defaultPrefs opts args
  where
    opts =
        info
            (commandParser <**> helper)
            ( fullDesc
                <> progDesc "Antithesis CLI"
                <> header "pop - A tool for managing Antithesis test runs using PoP"
            )

data Runtime = Runtime
    { httpCall :: Request -> IO (Response BL.ByteString)
    }

data PopConfig = PopConfig
    { tokenId :: String
    }
    deriving (Eq, Show)

data Result
    = RequestOK {txId :: TxId}
    | RegisterOK {userId :: String}
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

initPop :: IO PopConfig
initPop = pure $ PopConfig
    { tokenId = "register"
    }

conduitRuntime :: Runtime
conduitRuntime = Runtime
    { httpCall = Network.HTTP.Simple.httpLBS
    }

pop :: Runtime -> Args -> IO Result
pop runtime args = do
    config <- initPop
    parseArgs args >>= \case
        Request{platform, repository, commit, directory} -> runTest platform repository commit directory
        Register{platform, username, pubkeyhash} -> registerUser runtime (tokenId config) platform username pubkeyhash
        AddUser{platform, repository, role, userIdentifier} -> addUserToRepo platform repository role userIdentifier
        RemoveUser{platform, repository, userIdentifier} -> removeUserFromRepo platform repository userIdentifier

runTest :: Platform -> Repository -> SHA1 -> String -> IO Result
runTest _platform _repository _commit _directory = pure $ RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}

registerUser :: Runtime -> String -> Platform -> String -> String -> IO Result
registerUser runtime tokenId platform username pubkeyhash = do
    let url = "http://localhost:8080/token/" ++ tokenId ++ "/request"
        requestBody =
            object
                [ "key" .= [platform, username]
                , "value" .= pubkeyhash
                , "operation" .= ("insert" :: String)
                ]

    initialRequest <- parseRequest url
    let request =
            setRequestMethod "POST" $
                setRequestBodyLBS (encode requestBody) initialRequest

    response <- httpCall runtime request
    let statusCode = getResponseStatusCode response

    if statusCode >= 200 && statusCode < 300
        then do
            let responseBody = getResponseBody response
            -- In a real implementation, you would parse the response to get the txId
            pure $ RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}
        else error $ "HTTP request failed with status code: " ++ show statusCode

addUserToRepo :: Platform -> Repository -> String -> String -> IO Result
addUserToRepo _platform _repository _role _userIdentifier = pure $ RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}

removeUserFromRepo :: Platform -> Repository -> String -> IO Result
removeUserFromRepo _platform _repository _userIdentifier = pure $ RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}
