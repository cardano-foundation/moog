{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module User.Requester.CliSpec
    ( spec
    )
where

import Core.Context (withContext)
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , Duration (..)
    , FileName (..)
    , GithubRepository (..)
    , GithubUsername (..)
    , Owner (..)
    , Platform (..)
    , TokenId (..)
    )
import Core.Types.Fact (JSFact, keyHash, renderFacts, toJSFact)
import Core.Types.Mnemonics
    ( Mnemonics (ClearText)
    , MnemonicsPhase (..)
    )
import Core.Types.Tx
    ( TxHash (..)
    , WithTxHash (..)
    , WithUnsignedTx (..)
    )
import Core.Types.Wallet (Wallet (address))
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity (..))
import Data.String.QQ (s)
import Data.Text (Text)
import Lib.SSH.Private
    ( KeyPair (..)
    , SSHClient (..)
    , WithSelector (..)
    , mkKeyAPI
    )
import Lib.SSH.Public (makeSSHPublicKey)
import MPFS.API (MPFS (..))
import MockMPFS (mockMPFS, withFacts)
import Oracle.Config.Types (Config (..), ConfigKey (..))
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Requests.TestRun.Lib
    ( MockValidation (..)
    , gitAsset
    , mkEffects
    , noValidation
    )
import Oracle.Validate.Types (AValidationResult (..))
import Submitting (Submission (Submission), readWallet)
import Test.Hspec (Spec, describe, it, shouldBe)
import User.Agent.Types (TestRunId (TestRunId), WhiteListKey (..))
import User.Requester.Cli
    ( NewTestRunCreated (NewTestRunCreated)
    , RequesterCommand (..)
    , requesterCmd
    , signKey
    )
import User.Types
    ( GithubIdentification (IdentifyViaSSHKey)
    , Phase (..)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState (..)
    )

mnemonics :: Mnemonics 'DecryptedS
mnemonics =
    ClearText
        "coin april solid purity wish slight acquire kitchen dragon faculty clutch picnic"

wallet :: Wallet
wallet = case readWallet (True, mnemonics) of
    Left err -> error (show err)
    Right w -> w

sshClient :: SSHClient 'WithSelector
sshClient =
    SSHClient
        { sshKeySelector = Just "alice"
        , sshKeyFile = "alice_id_ed25519"
        , sshKeyPassphrase = "testpassphrase"
        }
testRepository :: GithubRepository
testRepository = GithubRepository{organization = "alice", project = "repo"}

testCommit :: Commit
testCommit = Commit "1234"

testDirectory :: Directory
testDirectory = Directory "tests"

testAssets :: [((GithubRepository, Maybe Commit, FileName), Text)]
testAssets =
    gitAsset testRun (FileName "README.md") "Test file"
        <> gitAsset testRun (FileName "docker-compose.yaml") "version: '3'"
        <> gitAsset testRun (FileName "testnet.yaml") "testnet: true"

testRun :: TestRun
testRun =
    TestRun
        { platform = testPlatform
        , repository = testRepository
        , directory = testDirectory
        , commitId = testCommit
        , tryIndex = 1
        , requester = GithubUsername "alice"
        }

testRunId :: TestRunId
testRunId = TestRunId $ case keyHash testRun of
    Nothing -> error "Failed to compute testRunId"
    Just h -> h

testPlatform :: Platform
testPlatform = Platform "github"

facts :: [JSFact]
facts = runIdentity $ do
    config <-
        toJSFact
            ConfigKey
            ( Config
                { configAgent = Owner "agent"
                , configTestRun =
                    TestRunValidationConfig
                        { minDuration = 1
                        , maxDuration = 10
                        }
                }
            )
            0
    role <-
        toJSFact
            ( RegisterRoleKey
                { platform = testPlatform
                , repository = testRepository
                , username = GithubUsername "alice"
                }
            )
            ()
            0
    user <-
        toJSFact
            ( RegisterUserKey
                { platform = testPlatform
                , username = GithubUsername "alice"
                , githubIdentification =
                    IdentifyViaSSHKey
                        $ makeSSHPublicKey
                            "AAAAC3NzaC1lZDI1NTE5AAAAIMNl2+jewJ5dBlIaOXM9PrPFpykU0mmzzQAAK/QF10Iy"
                }
            )
            ()
            0
    whiteList <-
        toJSFact
            ( WhiteListKey
                { platform = testPlatform
                , repository = testRepository
                }
            )
            ()
            0
    pure [config, role, whiteList, user]

aliceKey :: ByteString
aliceKey =
    [s|
-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAACmFlczI1Ni1jdHIAAAAGYmNyeXB0AAAAGAAAABAxi7TSyN
FNaswPaXSvyWCnAAAAGAAAAAEAAAAzAAAAC3NzaC1lZDI1NTE5AAAAIMNl2+jewJ5dBlIa
OXM9PrPFpykU0mmzzQAAK/QF10IyAAAAkDYmNoJPrd7TKP44DUhlI67GqE4PoEkEUYl/kR
l7nufsKSurucmJ//eJL0EgDrWabh7LZrnXs8EqOvn93N5ScL6uGAwe45nV9/hcq7at8yzs
wUKzoj1GlS881w5d1K9cXgaTNg2jXmtV3Mm/nYAZtxPXAp/9gxzUE2wWhQdi9NubBHIotl
8hEiM0+2SgxX/lMA==
-----END OPENSSH PRIVATE KEY-----
    |]

keyPair :: KeyPair
keyPair = case mkKeyAPI "testpassphrase" aliceKey (Just "alice") of
    Nothing -> error "Failed to create KeyPair"
    Just k -> k

validSSHKeys :: [(FilePath, ByteString)]
validSSHKeys = [("alice_id_ed25519", aliceKey)]

testDuration :: Duration
testDuration = Duration 3

pendingState :: TestRunState 'PendingT
pendingState = case signKey keyPair testRun of
    Nothing -> error "Failed to sign testRun"
    Just (_, signature) -> Pending testDuration signature

spec :: Spec
spec = describe "User.Requester.Cli" $ do
    it "should do something" $ do
        let tokenId = TokenId "token"
            command = RequestTest tokenId wallet (Just sshClient) testRun testDuration

        withContext
            mockMPFS{mpfsGetTokenFacts = const . pure $ renderFacts facts}
            ( \_ _ ->
                mkEffects
                    (withFacts facts mockMPFS)
                    $ noValidation
                        { mockSSHPrivateKey = validSSHKeys
                        , mockCommits = [(testRepository, testCommit)]
                        , mockDirectories =
                            [(testRepository, testCommit, testDirectory)]
                        , mockAssets = testAssets
                        }
            )
            mockSubmission
            (requesterCmd command)
            `shouldBe` Identity
                ( ValidationSuccess
                    ( WithTxHash mockTxHash
                        $ Just (NewTestRunCreated pendingState testRunId)
                    )
                )

mockTxHash :: TxHash
mockTxHash = TxHash "mock-tx-hash"

mockSubmission :: Wallet -> Submission Identity
mockSubmission w = Submission $ \f -> do
    WithUnsignedTx _ v <- f (address w)
    return $ WithTxHash mockTxHash v
