{-# LANGUAGE OverloadedRecordDot #-}

module Oracle.Validate.Requests.TestRun.Create
    ( validateCreateTestRun
    , validateCreateTestRunCore
    , TestRunRejection (..)
    , CreateTestRunFailure (..)
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , Duration (..)
    , GithubRepository (..)
    , GithubUsername (..)
    , Try (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Core.Types.Operation (Op (..), Operation (..))
import Core.Types.VKey (decodeVKey)
import Crypto.PubKey.Ed25519 (PublicKey)
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString.Lazy qualified as BL
import Data.List (find)
import Data.Maybe (mapMaybe)
import Effects
    ( Effects (..)
    , GithubEffects (..)
    , KeyFailure
    , hoistValidation
    , insertValidation
    )
import Lib.GitHub (GithubResponseError, GithubResponseStatusCodeError)
import Lib.JSON.Canonical.Extra (object, stringJSON, (.=))
import Lib.SSH.Public (decodeSSHPublicKey)
import Oracle.Types (requestZooGetTestRunKey)
import Oracle.Validate.DownloadAssets
    ( AssetValidationFailure
    , validateAssets
    )
import Oracle.Validate.Requests.Lib (keyAlreadyPendingFailure)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Types
    ( ForRole
    , Validate
    , Validated (..)
    , forUser
    , mapFailure
    , sequenceValidate
    , throwJusts
    )
import Text.JSON.Canonical
    ( ToJSON (..)
    , renderCanonicalJSON
    )
import User.Agent.Types (WhiteListKey (..))
import User.Types
    ( GithubIdentification (..)
    , Phase (PendingT)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState (..)
    , roleOfATestRun
    )

data CreateTestRunFailure
    = CreateTestRunRejections [TestRunRejection]
    | CreateTestRunKeyFailure KeyFailure
    | CreateTestConfigNotAvailable
    | CreateTestRunInvalidSSHKey
    | CreateTestRunKeyAlreadyPending TestRun
    | CreateTestRequesterNotRegistered
    deriving (Eq, Show)

instance Monad m => ToJSON m CreateTestRunFailure where
    toJSON (CreateTestRunRejections rejections) =
        object ["createTestRunRejections" .= rejections]
    toJSON (CreateTestRunKeyFailure keyFailure) =
        object ["createTestRunKeyFailure" .= keyFailure]
    toJSON CreateTestConfigNotAvailable =
        stringJSON "Token configuration is not available yet"
    toJSON CreateTestRunInvalidSSHKey =
        stringJSON "Invalid SSH key"
    toJSON (CreateTestRunKeyAlreadyPending testRun) =
        object ["createTestRunKeyAlreadyPending" .= testRun]
    toJSON CreateTestRequesterNotRegistered =
        stringJSON "Test requester is not registered"

validateCreateTestRun
    :: Monad m
    => TestRunValidationConfig
    -> Effects m
    -> ForRole
    -> Change TestRun (OpI (TestRunState PendingT))
    -> Validate CreateTestRunFailure m Validated
validateCreateTestRun
    testRunConfig
    validation
    forRole
    change@(Change (Key testRun) (Insert testRunState)) = do
        when (forUser forRole)
            $ keyAlreadyPendingFailure
                validation
                CreateTestRunKeyAlreadyPending
                testRun
                requestZooGetTestRunKey
        mapFailure CreateTestRunKeyFailure
            $ insertValidation validation change
        mapFailure CreateTestRunRejections
            $ validateCreateTestRunCore
                testRunConfig
                validation
                testRun
                testRunState

data TestRunRejection
    = UnacceptableDuration Int Int
    | UnacceptableCommit GithubRepository Commit
    | UnacceptableTryIndex Try
    | UnacceptableRole RegisterRoleKey
    | NoRegisteredKeyVerifiesTheSignature
    | UserIsNotRegistered
    | GithubResponseError GithubResponseError
    | GithubResponseStatusCodeError GithubResponseStatusCodeError
    | RepositoryNotWhitelisted
    | UnacceptableAssets AssetValidationFailure
    deriving (Eq, Show)

instance Monad m => ToJSON m TestRunRejection where
    toJSON (UnacceptableDuration minDuration maxDuration) =
        stringJSON
            $ "unacceptable duration. Expecting duration to be between "
                <> show minDuration
                <> " and "
                <> show maxDuration
    toJSON (UnacceptableCommit (GithubRepository org repo) (Commit commit)) =
        stringJSON
            $ "unacceptable commit. The specified commit "
                <> show commit
                <> " cannot be found in the repository "
                <> show org
                <> "/"
                <> show repo
    toJSON (UnacceptableTryIndex (Try maxIx)) =
        stringJSON
            $ "unacceptable try index. Expecting at most "
                <> show maxIx
                <> " run attempts for a given commit"
    toJSON
        ( UnacceptableRole
                (RegisterRoleKey _ (GithubRepository org repo) (GithubUsername user))
            ) =
            stringJSON
                $ "unacceptable role. User "
                    <> show user
                    <> " has not been registered within the repository "
                    <> show org
                    <> "/"
                    <> show repo
    toJSON NoRegisteredKeyVerifiesTheSignature =
        stringJSON
            "there is no registered Ed25519 SSH key that can verify the signature"
    toJSON UserIsNotRegistered =
        stringJSON "user has no Ed25519 SSH key registered"
    toJSON (GithubResponseError err) =
        object ["githubResponseError" .= err]
    toJSON (GithubResponseStatusCodeError err) =
        object ["githubResponseStatusCodeError" .= err]
    toJSON RepositoryNotWhitelisted =
        stringJSON "repository not whitelisted"
    toJSON (UnacceptableAssets failure) =
        object ["unacceptableAssets" .= failure]

checkDuration
    :: TestRunValidationConfig -> Duration -> Maybe TestRunRejection
checkDuration TestRunValidationConfig{maxDuration, minDuration} (Duration n)
    | n < minDuration || n > maxDuration =
        Just $ UnacceptableDuration minDuration maxDuration
    | otherwise = Nothing

checkRole
    :: Monad m => Effects m -> TestRun -> m (Maybe TestRunRejection)
checkRole
    Effects{mpfsGetFacts}
    testRun = do
        fs <- mpfsGetFacts
        let roleKey = roleOfATestRun testRun
        case find (\(Fact k () _) -> k == roleKey) fs of
            Just _ -> return Nothing
            Nothing -> return $ Just (UnacceptableRole roleKey)

checkWhiteList
    :: Monad m
    => Effects m
    -> TestRun
    -> m (Maybe TestRunRejection)
checkWhiteList
    Effects{mpfsGetFacts}
    testRun = do
        let proposed = WhiteListKey testRun.platform testRun.repository
        facts :: [Fact WhiteListKey ()] <- mpfsGetFacts
        if any (\(Fact k _ _) -> k == proposed) facts
            then return Nothing
            else return $ Just RepositoryNotWhitelisted

checkTryIndex
    :: Monad m
    => Effects m
    -> TestRun
    -> m (Maybe TestRunRejection)
checkTryIndex
    Effects{mpfsGetTestRuns}
    testRun = do
        testRuns :: [TestRun] <- mpfsGetTestRuns
        let sameCommitTestRuns =
                filter
                    ( \tr ->
                        tr.repository == testRun.repository
                            && commitId tr == commitId testRun
                            && tr.platform == testRun.platform
                            && directory tr == directory testRun
                    )
                    testRuns
            latest = case sameCommitTestRuns of
                [] -> Try 0
                _ -> maximum $ map tryIndex sameCommitTestRuns

        if tryIndex testRun == succ latest
            then return Nothing
            else return $ Just (UnacceptableTryIndex latest)

checkCommit
    :: Monad m
    => Effects m
    -> TestRun
    -> m (Maybe TestRunRejection)
checkCommit
    Effects{githubEffects = GithubEffects{githubCommitExists}}
    testRun = do
        existsE <- githubCommitExists testRun.repository (commitId testRun)
        pure $ case existsE of
            Left err -> Just $ GithubResponseError err
            Right exists ->
                if exists
                    then Nothing
                    else Just (UnacceptableCommit testRun.repository (commitId testRun))

checkSignature
    :: Monad m
    => Effects m
    -> TestRun
    -> Ed25519.Signature
    -> m (Maybe TestRunRejection)
checkSignature
    Effects{mpfsGetFacts}
    testRun
    signature = do
        registeredUsers <- mpfsGetFacts @_ @()
        testRunJ <- toJSON testRun
        let
            userKeys =
                mapMaybe
                    (decodeKey . githubIdentification)
                    . filter (\(RegisterUserKey _ u _) -> u == requester testRun)
                    $ factKey <$> registeredUsers
            load = BL.toStrict $ renderCanonicalJSON testRunJ
        if null userKeys
            then return $ Just UserIsNotRegistered
            else
                if any (\pk -> Ed25519.verify pk load signature) userKeys
                    then return Nothing
                    else return $ Just NoRegisteredKeyVerifiesTheSignature

decodeKey
    :: GithubIdentification
    -> Maybe PublicKey
decodeKey (IdentifyViaSSHKey sshKey) = decodeSSHPublicKey sshKey
decodeKey (IdentifyViaVKey vKey) = case decodeVKey vKey of
    Left _ -> Nothing
    Right pk -> Just pk

validateCreateTestRunCore
    :: Monad m
    => TestRunValidationConfig
    -> Effects m
    -> TestRun
    -> TestRunState PendingT
    -> Validate [TestRunRejection] m Validated
validateCreateTestRunCore
    config
    validation
    testRun
    (Pending duration _faultsEnabled signature) = do
        (withSystemTempDirectory $ hoistValidation validation)
            "moog-test-run"
            $ \tmpDir -> do
                let liftValidate f = lift (f validation testRun) >>= throwJusts
                Validated
                    <$ sequenceValidate
                        [ throwJusts $ checkDuration config duration
                        , liftValidate checkRole
                        , liftValidate checkTryIndex
                        , liftValidate checkCommit
                        , mapFailure UnacceptableAssets
                            $ validateAssets (Directory tmpDir) validation testRun
                        , liftValidate checkWhiteList
                        , liftValidate $ \v t -> checkSignature v t signature
                        ]
