module Facts
    ( FactsSelection (..)
    , TestRunSelection (..)
    , All (..)
    , tryDecryption
    , URLDecryptionIssue (..)
    , factsCmd
    )
where

import Control.Arrow (left)
import Control.Monad (filterM, when)
import Core.Types.Basic (GithubUsername)
import Core.Types.Fact (Fact (..), keyHash)
import Core.Types.VKey (DecodeVKeyError, decodeVKey)
import Core.Types.Wallet (Wallet, walletKeyPair)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as B8
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Effects (Effects (..))
import Lib.CryptoBox (decryptOnly)
import Lib.CryptoBox qualified as CB
import Lib.JSON.Canonical.Extra (blakeHashOfJSON)
import Lib.SSH.Private (KeyPair (..), SSHClient, WithSelector (..))
import Lib.SSH.Public (decodeSSHPublicKey)
import Oracle.Config.Types (Config, ConfigKey)
import Text.JSON.Canonical (FromJSON (..), JSValue, ToJSON (..))
import User.Agent.Types (TestRunId (..), WhiteListKey)
import User.Types
    ( GithubIdentification (..)
    , Phase (..)
    , RegisterRoleKey
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState (..)
    , URL (..)
    )

data All = All | Requester GithubUsername
    deriving (Eq, Show)
data TestRunSelection a where
    TestRunPending
        :: [TestRunId]
        -> All
        -> TestRunSelection [Fact TestRun (TestRunState 'PendingT)]
    TestRunRunning
        :: [TestRunId]
        -> All
        -> TestRunSelection [Fact TestRun (TestRunState 'RunningT)]
    TestRunDone
        :: Maybe Wallet
        -> Maybe (SSHClient 'WithSelector)
        -> [TestRunId]
        -> All
        -> TestRunSelection [Fact TestRun (TestRunState 'DoneT)]
    TestRunRejected
        :: [TestRunId]
        -> All
        -> TestRunSelection [Fact TestRun (TestRunState 'DoneT)]
    AnyTestRuns
        :: Maybe Wallet
        -> Maybe (SSHClient 'WithSelector)
        -> [TestRunId]
        -> All
        -> TestRunSelection [Fact TestRun JSValue]
data FactsSelection a where
    UserFacts :: FactsSelection [Fact RegisterUserKey ()]
    RoleFacts :: FactsSelection [Fact RegisterRoleKey ()]
    TestRunFacts :: TestRunSelection a -> FactsSelection a
    ConfigFact :: FactsSelection [Fact ConfigKey Config]
    WhiteListedFacts :: FactsSelection [Fact WhiteListKey ()]
    AllFacts :: FactsSelection [Fact JSValue JSValue]

filterFacts
    :: (Foldable t, ToJSON Identity k)
    => t TestRunId
    -> [Fact k v]
    -> [Fact k v]
filterFacts ids
    | null ids = id
    | otherwise =
        runIdentity
            . filterM (\v -> (`elem` ids) . TestRunId <$> keyHash (factKey v))

whoseFilter :: All -> [Fact TestRun v] -> [Fact TestRun v]
whoseFilter whose facts = filterOn facts factKey
    $ \case
        TestRun{requester} -> case whose of
            All -> True
            Requester u -> requester == u

factsCmd
    :: forall m a
     . Monad m
    => Effects m
    -> FactsSelection a
    -> m a
factsCmd effects selection = do
    let mkDecrypt mWallet mDecrypt = do
            decryption <-
                tryDecryption . fmap (factKey @RegisterUserKey @())
                    <$> mpfsGetFacts effects
            case (,) <$> mDecrypt <*> pure effects of
                Nothing -> pure $ decryption $ mWallet <&> walletKeyPair
                Just (ssh, _) ->
                    decryption <$> decodePrivateSSHFile effects ssh
    let
        testRunCommon
            :: FromJSON Maybe x => [TestRunId] -> All -> m [Fact TestRun x]
        testRunCommon ids whose =
            mpfsGetFacts effects
                <&> filterFacts ids . whoseFilter whose

        core UserFacts = mpfsGetFacts effects
        core RoleFacts = mpfsGetFacts effects
        core (TestRunFacts (TestRunPending ids whose)) = do
            testRunCommon ids whose
        core (TestRunFacts (TestRunRunning ids whose)) = do
            testRunCommon ids whose
        core (TestRunFacts (TestRunDone mWallet mDecrypt ids whose)) = do
            decrypt <- mkDecrypt mWallet mDecrypt
            facts <-
                testRunCommon ids whose <&> fmap decrypt
            pure $ filterOn facts factValue $ \case
                Finished{} -> True
                _ -> False
        core (TestRunFacts (TestRunRejected ids whose)) = do
            facts <-
                testRunCommon ids whose
            pure $ filterOn facts factValue $ \case
                Rejected{} -> True
                _ -> False
        core (TestRunFacts (AnyTestRuns mWallet mDecrypt ids whose)) = do
            decrypt <- mkDecrypt mWallet mDecrypt
            testRunCommon ids whose <&> fmap (parseDecrypt decrypt)
        core ConfigFact = mpfsGetFacts effects
        core WhiteListedFacts = mpfsGetFacts effects
        core AllFacts =
            mpfsGetFacts effects

    core selection

parseDecrypt
    :: ( Fact TestRun (TestRunState DoneT)
         -> Fact TestRun (TestRunState DoneT)
       )
    -> Fact TestRun JSValue
    -> Fact TestRun JSValue
parseDecrypt decrypt f =
    maybe f (fmap (runIdentity . toJSON) . decrypt) $ mapM fromJSON f

filterOn :: [a] -> (a -> b) -> (b -> Bool) -> [a]
filterOn xs f p = filter (p . f) xs

data URLDecryptionIssue
    = StateIsNotFinished
    | KeyDoesNotApply
    | SSHPublicKeyNotDecodable
    | VKeyNotDecodable DecodeVKeyError
    | URLNotBase64 String
    | NonceNotCreatable
    | KeyConversionsFailed String
    | URLDecryptionFailed
    | UsersNotRegistered GithubUsername

nothingLeft :: e -> Maybe a -> Either e a
nothingLeft e = maybe (Left e) Right

tryDecryption
    :: [RegisterUserKey]
    -> Maybe KeyPair
    -> Fact TestRun (TestRunState 'DoneT)
    -> Fact TestRun (TestRunState 'DoneT)
tryDecryption _ Nothing f = f
tryDecryption registeredUsers (Just kapi) f@(Fact tr ts slot) =
    case decryptURL registeredUsers tr ts kapi of
        Left _ -> f
        Right ts' -> Fact{factKey = tr, factValue = ts', factSlot = slot}

decryptURL
    :: [RegisterUserKey]
    -> TestRun
    -> TestRunState DoneT
    -> KeyPair
    -> Either URLDecryptionIssue (TestRunState DoneT)
decryptURL _ _ Rejected{} _ = Left StateIsNotFinished
decryptURL
    users
    testRun@TestRun{requester}
    (Finished old dur outcome (URL enc))
    KeyPair{privateKey, publicKey = providedPublicKey} = do
        RegisterUserKey{githubIdentification} <-
            nothingLeft (UsersNotRegistered requester)
                $ find ((== requester) . username) users
        testRunPublicKey <- case githubIdentification of
            IdentifyViaSSHKey ssh ->
                nothingLeft SSHPublicKeyNotDecodable
                    $ decodeSSHPublicKey ssh
            IdentifyViaVKey vkey ->
                left VKeyNotDecodable $ decodeVKey vkey
        when (testRunPublicKey /= providedPublicKey) $ Left KeyDoesNotApply
        decodedURL <- left URLNotBase64 $ Base64.decode $ B8.pack enc
        nonce <-
            nothingLeft NonceNotCreatable
                $ CB.mkNonce
                $ runIdentity
                $ blakeHashOfJSON testRun
        murl <-
            left KeyConversionsFailed $ decryptOnly privateKey decodedURL nonce
        url <- nothingLeft URLDecryptionFailed murl
        pure $ Finished old dur outcome $ URL $ B8.unpack url
