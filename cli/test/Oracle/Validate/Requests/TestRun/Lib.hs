{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Requests.TestRun.Lib
    ( jsFactRole
    , jsFactUser
    , mkValidation
    , noValidation
    , gitCommit
    , gitDirectory
    , signTestRun
    , changeTestRun
    , asciiStringL
    , positiveL
    , changePlatform
    , changeCommitId
    , changeDirectory
    , changeOrganization
    , changeProject
    , changeTry
    , changeRequester
    , testRunGen
    , signatureGen
    , testConfigGen
    , testConfigShrink
    , testConfigEGen
    , testRunEGen
    , MockValidation (..)
    , aToken
    , gitAsset
    )
where

import Control.Lens
    ( Lens'
    , Wrapped (_Wrapped')
    , (&)
    , (.~)
    , (^.)
    )
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , FileName (..)
    , Platform (Platform)
    , PublicKeyHash
    , Repository (..)
    , TokenId
    , Try (Try)
    , Username (Username)
    , organizationL
    , projectL
    )
import Core.Types.Fact
    ( JSFact
    , toJSFact
    )
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List qualified as L
import Data.Text (Text)
import Lib.GitHub
    ( GetGithubFileFailure (..)
    )
import Lib.SSH.Private (SSHClient (..), mkKeyAPI)
import Lib.SSH.Public (SSHPublicKey)
import MPFS.API (MPFS)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import System.Directory (Permissions (..), emptyPermissions)
import Test.QuickCheck
    ( ASCIIString (..)
    , Arbitrary (..)
    , Gen
    , Positive (..)
    , suchThat
    )
import Test.QuickCheck.Commit (CommitValue (..))
import Test.QuickCheck.Crypton ()
import Test.QuickCheck.EGen (EGen, gen, genShrink)
import Text.JSON.Canonical
    ( ToJSON (toJSON)
    , renderCanonicalJSON
    )
import User.Types
    ( RegisterRoleKey (RegisterRoleKey, platform, repository, username)
    , RegisterUserKey (RegisterUserKey, platform, pubkeyhash, username)
    , TestRun (..)
    , commitIdL
    , directoryL
    , platformL
    , repositoryL
    , requesterL
    , tryIndexL
    )
import Validation
    ( Validation (..)
    , getFacts
    , getTestRuns
    , getTokenRequests
    )
import Validation.DownloadFile
    ( DownloadedFileFailure (..)
    , analyzeDownloadedFile
    )
import Validation.RegisterRole (RepositoryRoleFailure (..))
import Validation.RegisterUser (analyzeKeys)

jsFactRole :: Monad m => TestRun -> m JSFact
jsFactRole testRun =
    toJSFact
        RegisterRoleKey
            { platform = testRun.platform
            , repository = testRun.repository
            , username = testRun.requester
            }
        ()

jsFactUser :: (Monad m) => TestRun -> PublicKeyHash -> m JSFact
jsFactUser testRun pubkeyhash =
    toJSFact
        RegisterUserKey
            { platform = testRun.platform
            , username = testRun.requester
            , pubkeyhash
            }
        ()

data MockValidation = MockValidation
    { mockCommits :: [(Repository, Commit)]
    , mockDirectories :: [(Repository, Commit, Directory)]
    , mockUserKeys :: [(Username, SSHPublicKey)]
    , mockRepoRoles :: [(Username, Repository)]
    , mockReposExists :: [Repository]
    , mockAssets :: [((Repository, Maybe Commit, FileName), Text)]
    , mockPermissions :: [(Directory, Permissions)]
    , mockSSHPrivateKey :: [(FilePath, ByteString)]
    }

aToken :: Maybe TokenId
aToken = Just $ error "TokenId not needed for tests"

mkValidation
    :: Monad m
    => MPFS m
    -> MockValidation
    -> Validation m
mkValidation
    mpfs
    MockValidation
        { mockCommits
        , mockDirectories
        , mockUserKeys
        , mockRepoRoles
        , mockReposExists
        , mockAssets
        , mockPermissions
        , mockSSHPrivateKey
        } =
        Validation
            { mpfsGetFacts = getFacts mpfs aToken
            , mpfsGetTestRuns = getTestRuns mpfs aToken
            , mpfsGetTokenRequests = getTokenRequests mpfs aToken
            , githubCommitExists = \repository commit ->
                return $ Right $ (repository, commit) `elem` mockCommits
            , githubDirectoryExists = \repository commit dir ->
                return $ Right $ (repository, commit, dir) `elem` mockDirectories
            , githubUserPublicKeys = \username publicKey ->
                pure
                    $ analyzeKeys publicKey
                    $ map snd
                    $ filter
                        ((== username) . fst)
                        mockUserKeys
            , githubRepositoryExists = \repo ->
                if repo `elem` mockReposExists
                    then pure $ Right True
                    else pure $ Right False
            , githubRepositoryRole = \username repository ->
                return
                    $ if (username, repository) `elem` mockRepoRoles
                        then Nothing
                        else Just NoRoleEntryInCodeowners
            , githubGetFile = \repository commit filename@(FileName name) ->
                case L.lookup (repository, commit, filename) mockAssets of
                    Nothing ->
                        return
                            $ Left
                            $ GithubGetFileError
                            $ GetGithubFileOtherFailure name "file not present"
                    Just filecontent ->
                        pure $ analyzeDownloadedFile filename (Right filecontent)
            , githubDownloadDirectory = \_ _ _ _ -> pure $ Right ()
            , directoryExists = \dir ->
                pure
                    $ dir
                        `lookup` ( mockPermissions
                                    <> [(Directory "tempdir", emptyPermissions{writable = True})]
                                 )
            , writeTextFile = \_path _content -> pure ()
            , withCurrentDirectory = \_dir action -> action
            , withSystemTempDirectory = \_template action -> action "tempdir"
            , decodePrivateSSHFile = \SSHClient{sshKeyFile, sshKeySelector, sshKeyPassphrase} ->
                case L.lookup sshKeyFile mockSSHPrivateKey of
                    Nothing -> pure Nothing
                    Just content -> pure $ mkKeyAPI sshKeyPassphrase content sshKeySelector
            }

testRunGen :: Gen TestRun
testRunGen = do
    ASCIIString platform <- arbitrary
    ASCIIString organization <- arbitrary
    ASCIIString project <- arbitrary
    ASCIIString directory <- arbitrary
    CommitValue commitId <- arbitrary
    Positive tryIndex <- arbitrary
    ASCIIString username <- arbitrary
    pure
        TestRun
            { platform = Platform platform
            , repository =
                Repository
                    { organization = organization
                    , project = project
                    }
            , directory = Directory directory
            , commitId = Commit commitId
            , tryIndex = Try tryIndex
            , requester = Username username
            }

testRunEGen :: EGen TestRun
testRunEGen = gen testRunGen

asciiStringL
    :: Functor f => (String -> f String) -> ASCIIString -> f ASCIIString
asciiStringL f s = ASCIIString <$> f (getASCIIString s)

positiveL :: Lens' (Positive Int) Int
positiveL f n = Positive <$> f (getPositive n)

changePlatform :: TestRun -> Gen TestRun
changePlatform = changeTestRun (platformL . _Wrapped') asciiStringL

changeCommitId :: TestRun -> Gen TestRun
changeCommitId = changeTestRun (commitIdL . _Wrapped') asciiStringL

changeDirectory :: TestRun -> Gen TestRun
changeDirectory = changeTestRun (directoryL . _Wrapped') asciiStringL

changeOrganization :: TestRun -> Gen TestRun
changeOrganization = changeTestRun (repositoryL . organizationL) asciiStringL

changeProject :: TestRun -> Gen TestRun
changeProject = changeTestRun (repositoryL . projectL) asciiStringL

changeTry :: TestRun -> Gen TestRun
changeTry = changeTestRun (tryIndexL . _Wrapped') positiveL

changeRequester :: TestRun -> Gen TestRun
changeRequester = changeTestRun (requesterL . _Wrapped') asciiStringL

changeTestRun
    :: (Arbitrary b, Eq a)
    => Lens' TestRun a
    -> Lens' b a
    -> TestRun
    -> Gen TestRun
changeTestRun l qc testRun = do
    let old = testRun ^. l
    new <- arbitrary `suchThat` \new -> new ^. qc /= old
    pure $ testRun & l .~ (new ^. qc)

noValidation :: MockValidation
noValidation =
    MockValidation
        { mockCommits = []
        , mockDirectories = []
        , mockUserKeys = []
        , mockRepoRoles = []
        , mockReposExists = []
        , mockAssets = []
        , mockPermissions = []
        , mockSSHPrivateKey = []
        }

gitCommit :: TestRun -> (Repository, Commit)
gitCommit testRun =
    ( testRun.repository
    , testRun.commitId
    )

gitDirectory :: TestRun -> (Repository, Commit, Directory)
gitDirectory testRun =
    ( testRun.repository
    , testRun.commitId
    , testRun.directory
    )

prefixed :: TestRun -> FileName -> FileName
prefixed testRun (FileName name) =
    let Directory prefix = directory testRun
    in  FileName $ prefix ++ "/" ++ name

gitAsset
    :: TestRun
    -> FileName
    -> Text
    -> [((Repository, Maybe Commit, FileName), Text)]
gitAsset testRun filename content =
    [
        ( (testRun.repository, Just testRun.commitId, prefixed testRun filename)
        , content
        )
    ]

signTestRun :: (Monad m, ToJSON m a) => (String -> b) -> a -> m b
signTestRun sign testRun = do
    testRunJ <- toJSON testRun
    pure $ sign $ BL.unpack $ renderCanonicalJSON testRunJ

signatureGen :: Gen Ed25519.Signature
signatureGen = do
    pk <- Ed25519.generateSecretKey
    pure $ Ed25519.sign pk (Ed25519.toPublic pk) ("hello" :: ByteString)

testConfigGen :: Gen TestRunValidationConfig
testConfigGen = do
    maxDuration <- arbitrary
    Positive minDuration <- arbitrary `suchThat` (<= maxDuration)
    pure
        $ TestRunValidationConfig
            { maxDuration = getPositive maxDuration
            , minDuration
            }

testConfigShrink
    :: TestRunValidationConfig -> [TestRunValidationConfig]
testConfigShrink config
    | config.maxDuration == config.minDuration = []
    | otherwise =
        [ TestRunValidationConfig
            { maxDuration = config.maxDuration
            , minDuration = minDuration'
            }
        | minDuration' <- [config.minDuration + 1 .. config.maxDuration]
        ]

testConfigEGen :: EGen TestRunValidationConfig
testConfigEGen = genShrink testConfigGen testConfigShrink
