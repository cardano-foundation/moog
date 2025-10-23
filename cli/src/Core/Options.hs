{-# LANGUAGE DerivingStrategies #-}

module Core.Options
    ( platformOption
    , repositoryOption
    , commitOption
    , testDirectoryOption
    , usernameOption
    , sshPublicKeyHashParser
    , vkeyOption
    , outputReferenceParser
    , durationOption
    , tryOption
    , tokenIdOption
    , downloadAssetsDirectoryOption
    , walletOption
    )
where

import Control.Arrow (left)
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , Duration (..)
    , GithubRepository (..)
    , GithubUsername (..)
    , Platform (..)
    , RequestRefId (..)
    , TokenId (..)
    , Try (..)
    )
import Core.Types.Mnemonics.Options (mnemonicsParser)
import Core.Types.Wallet
    ( Wallet
    )
import Data.CaseInsensitive (mk)
import Data.Text qualified as T
import Effects.RegisterUser (VKey (..))
import Lib.SSH.Public (SSHPublicKey, makeSSHPublicKey)
import OptEnvConf
    ( Parser
    , auto
    , checkEither
    , env
    , help
    , long
    , maybeReader
    , metavar
    , option
    , reader
    , setting
    , short
    , str
    , strOption
    )
import OptEnvConf.Reader (Reader (..))
import Submitting (readWallet)

platformOption :: Parser Platform
platformOption =
    Platform
        <$> strOption
            [ long "platform"
            , short 'p'
            , metavar "PLATFORM"
            , help "The platform to use"
            , option
            , env "MOOG_PLATFORM"
            , reader str
            ]

parseRepository :: String -> Maybe GithubRepository
parseRepository repoStr = case break (== '/') repoStr of
    (org, '/' : proj) ->
        Just $ GithubRepository{organization = mk org, project = mk proj}
    _ -> Nothing

repositoryOption :: Parser GithubRepository
repositoryOption =
    setting
        [ long "repository"
        , short 'r'
        , metavar "ORGANIZATION/PROJECT"
        , env "MOOG_REPOSITORY"
        , help "The repository in the format 'organization/project'"
        , reader (maybeReader parseRepository)
        , option
        ]

commitOption :: Parser Commit
commitOption =
    Commit
        <$> strOption
            [ long "commit"
            , short 'c'
            , metavar "COMMIT"
            , help "The commit hash or reference"
            ]

testDirectoryOption :: Parser Directory
testDirectoryOption =
    Directory
        <$> strOption
            [ long "directory"
            , short 'd'
            , metavar "DIRECTORY"
            , help "The repository directory where test assets are located"
            , option
            ]

downloadAssetsDirectoryOption :: Parser Directory
downloadAssetsDirectoryOption =
    Directory
        <$> strOption
            [ long "download-directory"
            , short 'D'
            , metavar "DIRECTORY"
            , help "The directory where assets will be downloaded to"
            , option
            ]

usernameOption :: Parser GithubUsername
usernameOption =
    GithubUsername
        <$> strOption
            [ long "username"
            , short 'u'
            , metavar "USERNAME"
            , env "MOOG_REQUESTER"
            , reader str
            , help "A github username"
            , option
            ]

sshPublicKeyHashParser :: Parser SSHPublicKey
sshPublicKeyHashParser =
    makeSSHPublicKey
        <$> strOption
            [ long "pubkeyhash"
            , short 'k'
            , metavar "PUBKEYHASH"
            , help "A public key hash for the user"
            , option
            ]

vkeyOption :: Parser VKey
vkeyOption =
    VKey
        <$> strOption
            [ long "vkey"
            , short 'v'
            , metavar "VKEY"
            , help "A bech32 Ed25519 verification key"
            , option
            ]

outputReferenceParser :: Parser RequestRefId
outputReferenceParser =
    setting
        [ long "outref"
        , short 'o'
        , metavar "OUTPUT_REF"
        , help "The transaction hash and index for the output reference"
        , reader parseOutputReference
        , option
        ]

parseOutputReference :: Reader RequestRefId
parseOutputReference = Reader $ \s -> do
    case break (== '-') s of
        (_txHash, '-' : indexStr) -> do
            _index :: Int <- case reads indexStr of
                [(i, "")] -> pure i
                _ ->
                    Left
                        "Invalid index format. Use 'txHash-index' where index is an integer."
            pure
                $ RequestRefId
                $ T.pack s
        _ -> Left "Invalid output reference format. Use 'txHash-index'"

durationOption :: Parser Duration
durationOption =
    Duration
        <$> setting
            [ long "duration"
            , short 't'
            , metavar "DURATION"
            , help "The duration in hours for the test-run"
            , reader auto
            , option
            ]

tryOption :: Parser Try
tryOption =
    Try
        <$> setting
            [ long "try"
            , short 'y'
            , metavar "TRY"
            , help "The current attempt number for this commit"
            , reader auto
            , option
            ]

-- If the token is not passed as the function argument, try to get it from the options
tokenIdOption :: Parser TokenId
tokenIdOption =
    TokenId
        <$> setting
            [ env "MOOG_TOKEN_ID"
            , metavar "TOKEN_ID"
            , help "The token ID of the antithesis token"
            , reader str
            ]

walletOption :: Parser Wallet
walletOption = checkEither (left show <$> readWallet) mnemonicsParser
