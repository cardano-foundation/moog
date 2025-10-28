{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module User.Requester.Cli
    ( requesterCmd
    , RequesterCommand (..)
    , signKey
    , NewTestRunCreated (..)
    ) where

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Core.Context
    ( WithContext
    , askConfig
    , askMpfs
    , askSubmit
    , askValidation
    )
import Core.Types.Basic
    ( Directory (..)
    , Duration
    , GithubRepository (..)
    , Success (..)
    , TokenId
    )
import Core.Types.Change (Change (..), Key (..), deleteKey, insertKey)
import Core.Types.Fact (Fact (..), keyHash)
import Core.Types.Operation (Operation (..))
import Core.Types.Tx (TxHash, WithTxHash (..), setWithTxHashValue)
import Core.Types.Wallet (Wallet, walletKeyPair)
import Crypto.PubKey.Ed25519 (Signature)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (find)
import Data.Functor (($>))
import Effects
    ( Effects (..)
    , GithubEffects (..)
    , getFacts
    , hoistValidation
    )
import Lib.GitHub (GetGithubFileFailure)
import Lib.JSON.Canonical.Extra (object, (.=))
import Lib.SSH.Private
    ( KeyPair (..)
    , SSHClient (..)
    , WithSelector (..)
    , sign
    )
import MPFS.API
    ( MPFS (..)
    , RequestDeleteBody (..)
    , RequestInsertBody (..)
    )
import Oracle.Config.Types (Config (..))
import Oracle.Validate.Requests.RegisterRole
    ( RegisterRoleFailure
    , UnregisterRoleFailure
    , validateRegisterRole
    , validateUnregisterRole
    )
import Oracle.Validate.Requests.RegisterUser
    ( RegisterUserFailure
    , UnregisterUserFailure
    , validateRegisterUser
    , validateUnregisterUser
    )
import Oracle.Validate.Requests.TestRun.Create
    ( CreateTestRunFailure (..)
    , validateCreateTestRun
    )
import Oracle.Validate.Types
    ( AValidationResult
    , ForRole (..)
    , liftMaybe
    , notValidated
    , runValidate
    , throwLeft
    )
import Submitting (Submission (..))
import Text.JSON.Canonical (JSValue, ToJSON (..), renderCanonicalJSON)
import User.Agent.Cli (TestRunId (..))
import User.Types
    ( GithubIdentification (..)
    , Phase (PendingT)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState (..)
    )

data RequesterCommand a where
    RegisterUser
        :: TokenId
        -> Wallet
        -> RegisterUserKey
        -> RequesterCommand (AValidationResult RegisterUserFailure TxHash)
    UnregisterUser
        :: TokenId
        -> Wallet
        -> RegisterUserKey
        -> RequesterCommand (AValidationResult UnregisterUserFailure TxHash)
    RegisterRole
        :: TokenId
        -> Wallet
        -> RegisterRoleKey
        -> RequesterCommand (AValidationResult RegisterRoleFailure TxHash)
    UnregisterRole
        :: TokenId
        -> Wallet
        -> RegisterRoleKey
        -> RequesterCommand (AValidationResult UnregisterRoleFailure TxHash)
    RequestTest
        :: TokenId
        -> Wallet
        -> Maybe (SSHClient 'WithSelector)
        -> TestRun
        -> Duration
        -> RequesterCommand
            ( AValidationResult
                CreateTestRunFailure
                (WithTxHash NewTestRunCreated)
            )
    GenerateAssets
        :: Directory
        -> RequesterCommand
            ( AValidationResult
                GetGithubFileFailure
                Success
            )

deriving instance Show (RequesterCommand a)
deriving instance Eq (RequesterCommand a)

requesterCmd
    :: Monad m
    => RequesterCommand a
    -> WithContext m a
requesterCmd command = do
    case command of
        RegisterUser tokenId wallet request ->
            registerUser tokenId wallet request
        UnregisterUser tokenId wallet request ->
            unregisterUser tokenId wallet request
        RegisterRole tokenId wallet request ->
            registerRole tokenId wallet request
        UnregisterRole tokenId wallet request ->
            unregisterRole tokenId wallet request
        RequestTest tokenId wallet mSshClient testRun duration ->
            createCommand
                tokenId
                wallet
                mSshClient
                testRun
                duration
        GenerateAssets directory -> generateAssets directory

generateAssets
    :: Monad m
    => Directory
    -> WithContext m (AValidationResult GetGithubFileFailure Success)
generateAssets (Directory targetDirectory) = do
    Effects
        { githubEffects = GithubEffects{githubDownloadDirectory}
        } <-
        askValidation Nothing
    lift
        $ runValidate
        $ do
            r <-
                lift
                    $ githubDownloadDirectory
                        (GithubRepository "cardano-foundation" "moog")
                        Nothing
                        (Directory "compose/testnets/cardano_node_master")
                        (Directory targetDirectory)
            throwLeft id r $> Success

signKey
    :: (ToJSON m key, Monad m) => KeyPair -> key -> m (JSValue, Signature)
signKey sshKey key = do
    jkey <- toJSON key
    pure (jkey, sign sshKey $ BL.toStrict $ renderCanonicalJSON jkey)

data NewTestRunCreated = NewTestRunCreated
    { newTestRunState :: TestRunState PendingT
    , newTestRunId :: TestRunId
    }
    deriving (Show, Eq)

instance Monad m => ToJSON m NewTestRunCreated where
    toJSON (NewTestRunCreated state (TestRunId hash)) =
        object
            [ "state" .= state
            , "testRunId" .= hash
            ]

createCommand
    :: Monad m
    => TokenId
    -> Wallet
    -> Maybe (SSHClient 'WithSelector)
    -> TestRun
    -> Duration
    -> WithContext
        m
        ( AValidationResult
            CreateTestRunFailure
            (WithTxHash NewTestRunCreated)
        )
createCommand
    tokenId
    wallet
    mSshClient
    testRun
    duration = do
        mconfig <- askConfig tokenId
        validation <- askValidation $ Just tokenId
        Submission submit <- askSubmit wallet
        mpfs <- askMpfs
        lift $ runValidate $ do
            users :: [Fact RegisterUserKey ()] <-
                lift $ getFacts mpfs $ Just tokenId
            user <- case find (\(Fact k _ _) -> k.username == requester testRun) users of
                Just (Fact k _ _) -> pure $ githubIdentification k
                Nothing -> notValidated CreateTestRequesterNotRegistered
            (key, signature) <- case (mSshClient, user) of
                (_, IdentifyViaVKey _) -> signKey (walletKeyPair wallet) testRun
                (Nothing, IdentifyViaSSHKey _) ->
                    notValidated CreateTestRunInvalidSSHKey
                (Just sshClient, IdentifyViaSSHKey _) -> do
                    sshKeyPair <-
                        liftMaybe CreateTestRunInvalidSSHKey
                            =<< decodePrivateSSHFile (hoistValidation validation) sshClient
                    lift $ signKey sshKeyPair testRun
            let newState = Pending duration signature
            Config{configTestRun} <-
                liftMaybe CreateTestConfigNotAvailable mconfig
            void
                $ validateCreateTestRun configTestRun validation ForUser
                $ Change (Key testRun) (Insert newState)
            value <- toJSON newState
            wtx <- lift $ submit $ \address -> do
                mpfsRequestInsert mpfs address tokenId
                    $ RequestInsertBody{key, value}
            hash <- keyHash testRun
            pure
                $ setWithTxHashValue wtx
                $ NewTestRunCreated
                    { newTestRunState = newState
                    , newTestRunId = TestRunId hash
                    }

registerUser
    :: Monad m
    => TokenId
    -> Wallet
    -> RegisterUserKey
    -> WithContext m (AValidationResult RegisterUserFailure TxHash)
registerUser
    tokenId
    wallet
    request = do
        mpfs <- askMpfs
        Submission submit <- askSubmit wallet
        validation <- askValidation $ Just tokenId
        lift $ runValidate $ do
            void
                $ validateRegisterUser validation ForUser
                $ insertKey request
            fmap txHash
                $ lift
                $ submit
                $ \address -> do
                    key <- toJSON request
                    value <- toJSON ()
                    mpfsRequestInsert mpfs address tokenId
                        $ RequestInsertBody{key = key, value = value}

unregisterUser
    :: Monad m
    => TokenId
    -> Wallet
    -> RegisterUserKey
    -> WithContext m (AValidationResult UnregisterUserFailure TxHash)
unregisterUser
    tokenId
    wallet
    request = do
        mpfs <- askMpfs
        validation <- askValidation $ Just tokenId
        Submission submit <- askSubmit wallet
        lift $ runValidate $ do
            void
                $ validateUnregisterUser validation ForUser
                $ deleteKey request
            fmap txHash
                $ lift
                $ submit
                $ \address -> do
                    key <- toJSON request
                    value <- toJSON ()
                    mpfsRequestDelete mpfs address tokenId
                        $ RequestDeleteBody{key = key, value = value}

registerRole
    :: Monad m
    => TokenId
    -> Wallet
    -> RegisterRoleKey
    -> WithContext m (AValidationResult RegisterRoleFailure TxHash)
registerRole
    tokenId
    wallet
    request = do
        mpfs <- askMpfs
        validation <- askValidation $ Just tokenId
        Submission submit <- askSubmit wallet
        lift $ runValidate $ do
            void
                $ validateRegisterRole validation ForUser
                $ insertKey request
            fmap txHash
                $ lift
                $ submit
                $ \address -> do
                    key <- toJSON request
                    value <- toJSON ()
                    mpfsRequestInsert mpfs address tokenId
                        $ RequestInsertBody{key = key, value = value}

unregisterRole
    :: Monad m
    => TokenId
    -> Wallet
    -> RegisterRoleKey
    -> WithContext m (AValidationResult UnregisterRoleFailure TxHash)
unregisterRole
    tokenId
    wallet
    request = do
        mpfs <- askMpfs
        validation <- askValidation $ Just tokenId
        Submission submit <- askSubmit wallet
        lift $ runValidate $ do
            void
                $ validateUnregisterRole validation ForUser
                $ deleteKey request
            fmap txHash
                $ lift
                $ submit
                $ \address -> do
                    key <- toJSON request
                    value <- toJSON ()
                    mpfsRequestDelete mpfs address tokenId
                        $ RequestDeleteBody{key = key, value = value}
