{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Requests.TestRun.Lib
    ( jsFactRole
    , jsFactUser
    , mkEffects
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
    , testConfigEGen
    , testRunEGen
    , MockValidation (..)
    , aToken
    , gitAsset
    , genDuration
    , testConfigFactGen
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
    , GithubRepository (..)
    , GithubUsername (GithubUsername)
    , Owner
    , Platform (Platform)
    , TokenId
    , Try (Try)
    , organizationL
    , projectL
    )
import Core.Types.Duration (Duration (..))
import Core.Types.Fact
    ( JSFact
    , toJSFact
    )
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.CaseInsensitive (CI (..), mk)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Effects
    ( Effects (..)
    , GithubEffects (..)
    , getFacts
    , getTestRuns
    , getTokenRequests
    )
import Effects.DownloadFile
    ( DownloadedFileFailure (..)
    , analyzeDownloadedFile
    )
import Effects.RegisterRole (RepositoryRoleFailure (..))
import Effects.RegisterUser
    ( VKey
    , VKeyFailure (..)
    , analyzeKeys
    , analyzeVKeyResponse
    )
import Lib.GitHub
    ( GetGithubFileFailure (..)
    )
import Lib.SSH.Private (SSHClient (..), mkKeyAPI)
import Lib.SSH.Public
    ( SSHPublicKey
    , renderSSHPublicKey
    )
import MPFS.API (MPFS)
import Oracle.Config.Types (ConfigKey (..), mkCurrentConfig)
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
import Test.QuickCheck.EGen (EGen, gen)
import Text.JSON.Canonical
    ( ToJSON (toJSON)
    , renderCanonicalJSON
    )
import User.Types
    ( GithubIdentification (..)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , commitIdL
    , directoryL
    , platformL
    , repositoryL
    , requesterL
    , tryIndexL
    )

jsFactRole :: Monad m => TestRun -> m JSFact
jsFactRole testRun =
    toJSFact
        RegisterRoleKey
            { platform = testRun.platform
            , repository = testRun.repository
            , username = testRun.requester
            }
        ()
        0

jsFactUser :: (Monad m) => TestRun -> GithubIdentification -> m JSFact
jsFactUser testRun githubIdentification =
    toJSFact
        RegisterUserKey
            { platform = testRun.platform
            , username = testRun.requester
            , githubIdentification
            }
        ()
        0

data MockValidation = MockValidation
    { mockCommits :: [(GithubRepository, Commit)]
    , mockDirectories :: [(GithubRepository, Commit, Directory)]
    , mockIdentifications :: [(GithubUsername, GithubIdentification)]
    , mockRepoRoles :: [(GithubUsername, GithubRepository)]
    , mockReposExists :: [GithubRepository]
    , mockAssets :: [((GithubRepository, Maybe Commit, FileName), Text)]
    , mockPermissions :: [(FilePath, Permissions)]
    , mockSSHPrivateKey :: [(FilePath, ByteString)]
    }

aToken :: Maybe TokenId
aToken = Just $ error "TokenId not needed for tests"

partitionIdentifications
    :: [(GithubUsername, GithubIdentification)]
    -> ( [(GithubUsername, SSHPublicKey)]
       , [(GithubUsername, VKey)]
       )
partitionIdentifications = foldr go ([], [])
  where
    go (username, IdentifyViaSSHKey pubkey) (sshKeys, vKeys) =
        ((username, pubkey) : sshKeys, vKeys)
    go (username, IdentifyViaVKey vkey) (sshKeys, vKeys) =
        (sshKeys, (username, vkey) : vKeys)

mkGithubEffects
    :: Monad m
    => MockValidation
    -> GithubEffects m
mkGithubEffects
    MockValidation
        { mockCommits
        , mockDirectories
        , mockIdentifications
        , mockRepoRoles
        , mockReposExists
        , mockAssets
        } =
        GithubEffects
            { githubCommitExists = \repository commit ->
                return $ Right $ (repository, commit) `elem` mockCommits
            , githubDirectoryExists = \repository commit dir ->
                return $ Right $ (repository, commit, dir) `elem` mockDirectories
            , githubUserPublicKeys = \username publicKey ->
                pure
                    $ analyzeKeys publicKey
                    $ map (T.pack . renderSSHPublicKey . snd)
                    $ filter
                        ((== username) . fst)
                    $ fst
                    $ partitionIdentifications mockIdentifications
            , githubUserVKeys = \username vkey ->
                pure
                    $ case lookup
                        username
                        $ snd
                        $ partitionIdentifications
                            mockIdentifications of
                        Nothing -> Just VKeyNotFound
                        Just actualVKey -> analyzeVKeyResponse vkey (Right actualVKey)
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
            }

mkEffects
    :: Monad m
    => MPFS m
    -> MockValidation
    -> Effects m
mkEffects
    mpfs
    mock@MockValidation
        { mockPermissions
        , mockSSHPrivateKey
        } =
        Effects
            { mpfsGetFacts = getFacts mpfs aToken
            , mpfsGetTestRuns = getTestRuns mpfs aToken
            , mpfsGetTokenRequests = getTokenRequests mpfs aToken
            , githubEffects = mkGithubEffects mock
            , directoryExists = \(Directory dir) ->
                pure
                    $ dir
                        `lookup` ( mockPermissions
                                    <> [("tempdir", emptyPermissions{writable = True})]
                                 )
            , fileExists = \filePath ->
                pure
                    $ filePath
                        `lookup` ( mockPermissions
                                    <> [("tempdir/docker-compose.yml", emptyPermissions{writable = True})]
                                 )
            , writeTextFile = \_path _content -> pure ()
            , withCurrentDirectory = \_dir action -> action
            , withSystemTempDirectory = \_template action -> action "tempdir"
            , decodePrivateSSHFile = \SSHClient{sshKeyFile, sshKeySelector, sshKeyPassphrase} ->
                case L.lookup sshKeyFile mockSSHPrivateKey of
                    Nothing -> pure Nothing
                    Just content -> pure $ mkKeyAPI sshKeyPassphrase content sshKeySelector
            , dockerComposeConfigure = \_dir -> pure $ Right ()
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
                GithubRepository
                    { organization = mk organization
                    , project = mk project
                    }
            , directory = Directory directory
            , commitId = Commit commitId
            , tryIndex = Try tryIndex
            , requester = GithubUsername $ mk username
            }

testRunEGen :: EGen TestRun
testRunEGen = gen testRunGen

brokenCIL :: Lens' String (CI String)
brokenCIL f s = foldedCase <$> f (mk s)

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
changeOrganization =
    changeTestRun
        (repositoryL . organizationL)
        (asciiStringL . brokenCIL)

changeProject :: TestRun -> Gen TestRun
changeProject = changeTestRun (repositoryL . projectL) (asciiStringL . brokenCIL)

changeTry :: TestRun -> Gen TestRun
changeTry = changeTestRun (tryIndexL . _Wrapped') positiveL

changeRequester :: TestRun -> Gen TestRun
changeRequester = changeTestRun (requesterL . _Wrapped') (asciiStringL . brokenCIL)

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
        , mockIdentifications = []
        , mockRepoRoles = []
        , mockReposExists = []
        , mockAssets = []
        , mockPermissions = []
        , mockSSHPrivateKey = []
        }

gitCommit :: TestRun -> (GithubRepository, Commit)
gitCommit testRun =
    ( testRun.repository
    , testRun.commitId
    )

gitDirectory :: TestRun -> (GithubRepository, Commit, Directory)
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
    -> [((GithubRepository, Maybe Commit, FileName), Text)]
gitAsset testRun filename content =
    [
        ( (testRun.repository, Just testRun.commitId, prefixed testRun filename)
        , content
        )
    ]

genDuration :: Gen Duration
genDuration = do
    hoursOrMinutes <- arbitrary
    if hoursOrMinutes
        then do
            Hours <$> arbitrary
        else do
            Minutes <$> arbitrary

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
    minDuration <- genDuration `suchThat` (>= mempty)
    maxDuration <- genDuration `suchThat` (> minDuration)

    pure
        $ TestRunValidationConfig
            { maxDuration
            , minDuration
            }

testConfigEGen :: EGen TestRunValidationConfig
testConfigEGen = gen testConfigGen

testConfigFactGen :: Owner -> EGen JSFact
testConfigFactGen owner = do
    testConfig <- testConfigEGen
    toJSFact ConfigKey (mkCurrentConfig owner testConfig) 0
