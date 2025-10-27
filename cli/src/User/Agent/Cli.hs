{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module User.Agent.Cli
    ( AgentCommand (..)
    , TestRunId (..)
    , IsReady (..)
    , CheckResultsFailure (..)
    , ReportFailure (..)
    , agentCmd
    )
where

import Control.Applicative (asum)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Core.Context
    ( WithContext
    , askConfig
    , askMpfs
    , askSubmit
    , askValidation
    , withMPFS
    )
import Core.Types.Basic
    ( Directory
    , Duration
    , GithubRepository
    , GithubUsername (..)
    , Owner
    , Platform
    , Success (..)
    , TokenId
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact
    ( Fact (..)
    , keyHash
    , parseFacts
    )
import Core.Types.Operation (Op (..), Operation (..))
import Core.Types.Tx (WithTxHash (..), setWithTxHashValue)
import Core.Types.Wallet (Wallet (..))
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as B8
import Data.CaseInsensitive (CI (..))
import Data.Either (rights)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.Functor.Identity (Identity (..))
import Data.List (find)
import Effects (Effects)
import Lib.CryptoBox qualified as CB
import Lib.JSON.Canonical.Extra (blakeHashOfJSON, object, (.=))
import MPFS.API
    ( MPFS (..)
    , RequestDeleteBody (..)
    , RequestInsertBody (..)
    , RequestUpdateBody (..)
    )
import Oracle.Config.Types (Config (..))
import Oracle.Validate.DownloadAssets
    ( DownloadAssetsFailure
    , validateDownloadAssets
    )
import Oracle.Validate.Requests.ManageWhiteList
    ( UpdateWhiteListFailure (..)
    , validateAddWhiteListed
    , validateRemoveWhiteListed
    )
import Oracle.Validate.Requests.TestRun.Update
    ( UpdateTestRunFailure (..)
    , validateToDoneUpdate
    , validateToRunningUpdate
    )
import Oracle.Validate.Types
    ( AValidationResult (..)
    , ForRole (..)
    , Validate
    , Validated
    , hoistValidate
    , liftMaybe
    , mapFailure
    , notValidated
    , runValidate
    , throwLeft
    )
import Streaming.Prelude qualified as S
import Submitting (Submission (..))
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ToJSON (..)
    )
import User.Agent.PublishResults.Email
    ( EmailException
    , EmailPassword
    , EmailUser
    , Result (..)
    , readEmails
    )
import User.Agent.PushTest
    ( AntithesisAuth
    , PushFailure (..)
    , Registry
    , SlackWebhook
    , pushTestToAntithesisIO
    )
import User.Agent.Types
    ( TestRunId (..)
    , TestRunMap (..)
    , TestRunStatus (..)
    , WhiteListKey (..)
    )
import User.Types
    ( Phase (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunRejection
    , TestRunState (..)
    , URL (..)
    , getIdentificationPublicKey, Outcome
    )

type ValidateWithContext m a =
    Validate UpdateTestRunFailure (WithContext m) a

findFact
    :: Monad m
    => TokenId
    -> TestRunId
    -> WithContext m (Maybe (Fact TestRun JSValue))
findFact tk (TestRunId testRunId) = do
    facts <- fmap parseFacts
        $ withMPFS
        $ \mpfs -> mpfsGetTokenFacts mpfs tk
    let match :: Fact TestRun JSValue -> Bool
        match (Fact key _) = case keyHash key of
            Nothing -> False
            Just keyId -> keyId == testRunId
    pure $ find match facts

data CheckResultsFailure
    = CheckResultsNoTestRunFor TestRunId
    | CheckResultsEmail EmailException
    | CheckResultsNoEmailsFor TestRunId
    deriving (Show, Eq)

instance Monad m => ToJSON m CheckResultsFailure where
    toJSON (CheckResultsNoTestRunFor (TestRunId trId)) =
        object
            [ "error" .= ("No test run for given id" :: String)
            , "testRunId" .= trId
            ]
    toJSON (CheckResultsEmail err) =
        object
            [ "error" .= ("Email error" :: String)
            , "details" .= show err
            ]
    toJSON (CheckResultsNoEmailsFor (TestRunId trId)) =
        object
            [ "error" .= ("No emails found for given test run id" :: String)
            , "testRunId" .= trId
            ]

withPreviousTestRunState
    :: (Monad m, FromJSON Maybe (TestRunState phase))
    => TokenId
    -> TestRunId
    -> (Fact TestRun (TestRunState phase) -> ValidateWithContext m a)
    -> ValidateWithContext m a
withPreviousTestRunState tk testRunId cont = do
    mfact <- lift $ findFact tk testRunId
    fact <-
        liftMaybe UpdateTestRunTestRunIdNotResolved mfact
    value <-
        liftMaybe UpdateTestRunWrongPreviousState
            $ fromJSON
            $ factValue fact
    cont fact{factValue = value}

updateTestRunState
    :: (Monad m, FromJSON Maybe (TestRunState phase))
    => TokenId
    -> TestRunId
    -> ( Fact TestRun (TestRunState phase)
         -> ValidateWithContext m a
       )
    -> Validate UpdateTestRunFailure (WithContext m) a
updateTestRunState = withPreviousTestRunState

agentCmd
    :: MonadIO m
    => AgentCommand NotReady a
    -> WithContext m a
agentCmd = \case
    Query tokenId -> queryCommand tokenId
    WhiteList tokenId wallet platform repo ->
        whiteList tokenId wallet platform repo
    BlackList tokenId wallet platform repo ->
        blackList tokenId wallet platform repo
    DownloadAssets tokenId key dir ->
        ($> Success) <$> downloadAssets tokenId key dir
    Accept tokenId wallet key () -> runValidate
        $ updateTestRunState tokenId key
        $ \fact ->
            acceptCommand tokenId wallet fact
    Reject tokenId wallet key () reason -> runValidate
        $ updateTestRunState tokenId key
        $ \fact ->
            rejectCommand tokenId wallet fact reason
    Report tokenId wallet key () duration outcome (URL urlText) -> runValidate $ do
        reportCommand tokenId wallet key duration outcome urlText
    PushTest tokenId registry auth dir key slack -> runValidate $ do
        pushTestToAntithesisIO
            tokenId
            registry
            auth
            dir
            key
            slack
            $> Success
    CheckResultFor tk emailUser emailPassword key days -> runValidate $ do
        mfact <- lift $ findFact tk key
        Fact testRun' _ <-
            liftMaybe (CheckResultsNoTestRunFor key) mfact
        r <- liftIO $ do
            let onlyResults (Right r@Result{description}) = do
                    when (description == testRun') $ S.yield r
                onlyResults (Left _) = pure ()
            runExceptT
                $ readEmails emailUser emailPassword days
                & flip S.for onlyResults
                & S.head_

        case r of
            Left err -> notValidated $ CheckResultsEmail err
            Right Nothing -> notValidated $ CheckResultsNoEmailsFor key
            Right (Just result) -> pure result
    CheckAllResults emailUser emailPassword days -> runValidate $ do
        r <-
            liftIO
                $ runExceptT
                $ readEmails emailUser emailPassword days
                & S.toList_
        case r of
            Left err -> notValidated $ CheckResultsEmail err
            Right results -> pure $ rights results

data IsReady = NotReady | Ready
    deriving (Show, Eq)

type family IfReady a b where
    IfReady NotReady _ = ()
    IfReady Ready b = b

data Role = Internal | External
    deriving (Show, Eq)

type family ResolveId phase where
    ResolveId NotReady = TestRunId
    ResolveId Ready = TestRun

data ReportFailure
    = ReportFailureUserKeyNotFound GithubUsername
    | ReportFailureUserKeyUnparsable GithubUsername
    | ReportFailureFactNotFound TestRunId
    | ReportFailureUpdate UpdateTestRunFailure
    | ReportFailureFailToEncrypt String
    | ReportFailureNonceCreationFailed
    deriving (Show, Eq)

instance Monad m => ToJSON m ReportFailure where
    toJSON (ReportFailureUserKeyNotFound (GithubUsername user)) =
        object
            [ "userKeyNotFound" .= foldedCase user
            ]
    toJSON (ReportFailureUserKeyUnparsable (GithubUsername user)) =
        object
            [ "userKeyUnparsable" .= foldedCase user
            ]
    toJSON (ReportFailureFactNotFound (TestRunId trId)) =
        object
            [ "factNotFound" .= trId
            ]
    toJSON (ReportFailureUpdate err) =
        object
            [ "updateError" .= err
            ]
    toJSON (ReportFailureFailToEncrypt err) =
        object
            [ "failToEncrypt" .= err
            ]
    toJSON ReportFailureNonceCreationFailed =
        pure
            $ JSString "nonceCreationFailed"

data AgentCommand (phase :: IsReady) result where
    Accept
        :: TokenId
        -> Wallet
        -> ResolveId phase
        -> IfReady phase (TestRunState PendingT)
        -> AgentCommand
            phase
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState RunningT))
            )
    Reject
        :: TokenId
        -> Wallet
        -> ResolveId phase
        -> IfReady phase (TestRunState PendingT)
        -> [TestRunRejection]
        -> AgentCommand
            phase
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState DoneT))
            )
    Report
        :: TokenId
        -> Wallet
        -> ResolveId phase
        -> IfReady phase (TestRunState RunningT)
        -> Duration
        -> Outcome
        -> URL
        -> AgentCommand
            phase
            ( AValidationResult
                ReportFailure
                (WithTxHash (TestRunState DoneT))
            )
    Query :: TokenId -> AgentCommand phase TestRunMap
    WhiteList
        :: TokenId
        -> Wallet
        -> Platform
        -> GithubRepository
        -> AgentCommand
            phase
            (AValidationResult UpdateWhiteListFailure (WithTxHash Success))
    BlackList
        :: TokenId
        -> Wallet
        -> Platform
        -> GithubRepository
        -> AgentCommand
            phase
            (AValidationResult UpdateWhiteListFailure (WithTxHash Success))
    DownloadAssets
        :: TokenId
        -> TestRunId
        -> Directory
        -> AgentCommand
            phase
            (AValidationResult DownloadAssetsFailure Success)
    CheckResultFor
        :: TokenId
        -> EmailUser
        -> EmailPassword
        -> TestRunId
        -> Int
        -- ^ limit to last N days
        -> AgentCommand phase (AValidationResult CheckResultsFailure Result)
    CheckAllResults
        :: EmailUser
        -> EmailPassword
        -> Int
        -- ^ limit to last N days
        -> AgentCommand phase (AValidationResult CheckResultsFailure [Result])
    PushTest
        :: TokenId
        -> Registry
        -> AntithesisAuth
        -> Directory
        -> TestRunId
        -> Maybe SlackWebhook
        -> AgentCommand
            phase
            (AValidationResult PushFailure Success)

deriving instance Show (AgentCommand NotReady result)
deriving instance Eq (AgentCommand NotReady result)
deriving instance Show (AgentCommand Ready result)
deriving instance Eq (AgentCommand Ready result)

whiteList
    :: Monad m
    => TokenId
    -> Wallet
    -> Platform
    -> GithubRepository
    -> WithContext
        m
        ( AValidationResult
            UpdateWhiteListFailure
            (WithTxHash Success)
        )
whiteList tokenId wallet platform repo = do
    let key = WhiteListKey platform repo
        change =
            Change
                { key = Key key
                , operation = Insert ()
                }
        requester = owner wallet
    validation <- askValidation $ Just tokenId
    Submission submit <- askSubmit wallet
    mpfs <- askMpfs
    mconfig <- askConfig tokenId
    lift $ runValidate $ do
        Config{configAgent} <- liftMaybe WhiteListConfigNotAvailable mconfig
        void $ validateAddWhiteListed validation requester configAgent change
        wtx <- lift $ submit $ \address -> do
            jkey <- toJSON key
            mpfsRequestInsert mpfs address tokenId
                $ RequestInsertBody{key = jkey, value = JSNull}
        pure $ setWithTxHashValue wtx Success

blackList
    :: Monad m
    => TokenId
    -> Wallet
    -> Platform
    -> GithubRepository
    -> WithContext
        m
        ( AValidationResult
            UpdateWhiteListFailure
            (WithTxHash Success)
        )
blackList tokenId wallet platform repo = do
    let key = WhiteListKey platform repo
        change = Change (Key key) (Delete ())
        requester = owner wallet
    validation <- askValidation $ Just tokenId
    Submission submit <- askSubmit wallet
    mpfs <- askMpfs
    mconfig <- askConfig tokenId
    lift $ runValidate $ do
        Config{configAgent} <- liftMaybe WhiteListConfigNotAvailable mconfig
        void
            $ validateRemoveWhiteListed validation requester configAgent change
        wtx <- lift $ submit $ \address -> do
            jkey <- toJSON key
            mpfsRequestDelete mpfs address tokenId
                $ RequestDeleteBody{key = jkey, value = JSNull}
        pure $ setWithTxHashValue wtx Success

queryCommand :: Monad m => TokenId -> WithContext m TestRunMap
queryCommand tokenId = do
    facts <- withMPFS $ \mpfs -> mpfsGetTokenFacts mpfs tokenId
    let testRunsPending = parseFacts facts
        testRunsRunning = parseFacts facts
        testRunsDone = parseFacts facts
    pure
        $ TestRunMap
            { pending = testRunsPending <&> StatusPending
            , running = testRunsRunning <&> StatusRunning
            , done = testRunsDone <&> StatusDone
            }

downloadAssets
    :: Monad m
    => TokenId
    -> TestRunId
    -> Directory
    -> WithContext m (AValidationResult DownloadAssetsFailure ())
downloadAssets tokenId testRunId dir = do
    testmap <- queryCommand tokenId
    validation <- askValidation $ Just tokenId
    lift $ runValidate $ do
        void $ validateDownloadAssets validation testmap testRunId dir

signAndSubmitAnUpdate
    :: (ToJSON m key, ToJSON m old, ToJSON m new, Monad m)
    => ( Effects m
         -> Owner
         -> Owner
         -> Change key ('OpU old new)
         -> Validate UpdateTestRunFailure m Validated
       )
    -> TokenId
    -> Wallet
    -> Fact key old
    -> new
    -> ValidateWithContext m (WithTxHash new)
signAndSubmitAnUpdate validate tokenId wallet (Fact testRun oldState) newState = do
    let requester = owner wallet
    validation <- lift $ askValidation $ Just tokenId
    mconfig <- lift $ askConfig tokenId
    Submission submit <- lift $ askSubmit wallet
    mpfs <- lift askMpfs
    Config{configAgent} <-
        liftMaybe UpdateTestRunConfigNotAvailable mconfig
    void
        $ hoistValidate lift
        $ validate validation configAgent requester
        $ Change (Key testRun)
        $ Update oldState newState
    wtx <- lift $ lift $ submit $ \address -> do
        key <- toJSON testRun
        oldValue <- toJSON oldState
        newValue <- toJSON newState
        mpfsRequestUpdate mpfs address tokenId
            $ RequestUpdateBody{key, oldValue, newValue}
    pure $ setWithTxHashValue wtx newState

reportTxCommand
    :: Monad m
    => TokenId
    -> Wallet
    -> Fact TestRun (TestRunState RunningT)
    -> Duration
    -> Outcome
    -> URL
    -> ValidateWithContext
        m
        (WithTxHash (TestRunState DoneT))
reportTxCommand tokenId wallet fact duration outcome url =
    signAndSubmitAnUpdate
        (`validateToDoneUpdate` ForUser)
        tokenId
        wallet
        fact
        $ Finished (factValue fact) duration outcome url

rejectCommand
    :: Monad m
    => TokenId
    -> Wallet
    -> Fact TestRun (TestRunState PendingT)
    -> [TestRunRejection]
    -> ValidateWithContext
        m
        (WithTxHash (TestRunState DoneT))
rejectCommand tokenId wallet fact reason =
    signAndSubmitAnUpdate
        (`validateToDoneUpdate` ForUser)
        tokenId
        wallet
        fact
        $ Rejected (factValue fact) reason

acceptCommand
    :: Monad m
    => TokenId
    -> Wallet
    -> Fact TestRun (TestRunState PendingT)
    -> ValidateWithContext
        m
        (WithTxHash (TestRunState RunningT))
acceptCommand tokenId wallet fact =
    signAndSubmitAnUpdate
        (`validateToRunningUpdate` ForUser)
        tokenId
        wallet
        fact
        $ Accepted (factValue fact)

reportCommand
    :: Monad m
    => TokenId
    -> Wallet
    -> TestRunId
    -> Duration
    -> Outcome
    -> String
    -> Validate
        ReportFailure
        (WithContext m)
        (WithTxHash (TestRunState DoneT))
reportCommand tokenId wallet key duration outcome urlText = do
    encryptedUrl <- encryptForRequester tokenId key urlText
    mapFailure ReportFailureUpdate
        $ updateTestRunState tokenId key
        $ \fact ->
            reportTxCommand tokenId wallet fact duration outcome
                $ URL
                $ B8.unpack . Base64.encode
                $ encryptedUrl

encryptForRequester
    :: Monad m
    => TokenId
    -> TestRunId
    -> String
    -> Validate ReportFailure (WithContext m) B8.ByteString
encryptForRequester tokenId key urlText = do
    mfact <- lift $ findFact tokenId key
    Fact testRun _ <-
        liftMaybe (ReportFailureFactNotFound key) mfact
    let user = requester testRun
    users <- lift
        $ fmap (fmap factKey . parseFacts @_ @())
        $ withMPFS
        $ \mpfs -> mpfsGetTokenFacts mpfs tokenId
    let match (RegisterUserKey _ u k)
            | u == user = Just k
            | otherwise = Nothing
    userKey <-
        liftMaybe (ReportFailureUserKeyNotFound user)
            $ asum
            $ fmap match users
    pk <-
        liftMaybe (ReportFailureUserKeyUnparsable user)
            $ getIdentificationPublicKey userKey
    nonce <-
        liftMaybe ReportFailureNonceCreationFailed
            $ CB.mkNonce
            $ runIdentity
            $ blakeHashOfJSON testRun
    throwLeft ReportFailureFailToEncrypt
        $ CB.encryptOnly pk (B8.pack urlText) nonce
