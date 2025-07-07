{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module User.Agent.Cli
    ( AgentCommand (..)
    , IsReady (..)
    , agentCmd
    )
where

import Core.Types (TokenId, Wallet, WithTxHash (..))
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import MPFS.API
    ( RequestInsertBody (..)
    , RequestUpdateBody (..)
    , getTokenFacts
    , requestInsert
    , requestUpdate
    )
import Servant.Client (ClientM)
import Submitting (Submitting, signAndSubmit)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ToJSON (..)
    , renderCanonicalJSON
    , toJSString
    )
import User.Types
    ( Duration
    , Phase (..)
    , Reason
    , TestRun (..)
    , TestRunState (..)
    , URL (..)
    )

agentCmd
    :: Submitting
    -> Wallet
    -> TokenId
    -> AgentCommand NotReady a
    -> ClientM a
agentCmd sbmt wallet tokenId cmdNotReady = do
    mCmdReady <- resolveOldState tokenId cmdNotReady
    case mCmdReady of
        Nothing -> error "No previous state found for the command"
        Just cmdReady -> agentCmdCore sbmt wallet tokenId cmdReady

withExpectedState
    :: FromJSON Maybe (TestRunState phase)
    => TokenId
    -> TestRun
    -> (TestRunState phase -> ClientM result)
    -> ClientM (Maybe result)
withExpectedState tokenId testRun cont = do
    facts <- getTokenFacts tokenId
    jsonKeyValue <- toJSON testRun
    let jsonKey =
            toJSString
                $ T.unpack
                $ decodeUtf8
                $ BL.toStrict
                $ renderCanonicalJSON jsonKeyValue
    case facts of
        JSObject mapping -> do
            case lookup jsonKey mapping of
                Just value -> do
                    case fromJSON value of
                        Nothing -> pure Nothing
                        Just x -> Just <$> cont x
                Nothing -> pure Nothing
        _ -> pure Nothing

resolveOldState
    :: TokenId
    -> AgentCommand NotReady result
    -> ClientM (Maybe (AgentCommand Ready result))
resolveOldState tokenId cmd = case cmd of
    Create key duration -> pure $ Just $ Create key duration
    Accept key () -> withExpectedState tokenId key $ pure . Accept key
    Reject key () reason ->
        withExpectedState tokenId key
            $ \pending -> pure $ Reject key pending reason
    Report key () duration url ->
        withExpectedState tokenId key $ \runningState ->
            pure $ Report key runningState duration url

data IsReady = NotReady | Ready
    deriving (Show, Eq)

type family IfReady a b where
    IfReady NotReady _ = ()
    IfReady Ready b = b

data Role = Internal | External
    deriving (Show, Eq)

data AgentCommand (phase :: IsReady) result where
    Create
        :: TestRun
        -> Duration
        -> AgentCommand phase (WithTxHash (TestRunState PendingT))
    Accept
        :: TestRun
        -> IfReady phase (TestRunState PendingT)
        -> AgentCommand phase (WithTxHash (TestRunState RunningT))
    Reject
        :: TestRun
        -> IfReady phase (TestRunState PendingT)
        -> [Reason]
        -> AgentCommand phase (WithTxHash (TestRunState DoneT))
    Report
        :: TestRun
        -> IfReady phase (TestRunState RunningT)
        -> Duration
        -> URL
        -> AgentCommand phase (WithTxHash (TestRunState DoneT))

deriving instance Show (AgentCommand NotReady result)
deriving instance Eq (AgentCommand NotReady result)
deriving instance Show (AgentCommand Ready result)
deriving instance Eq (AgentCommand Ready result)

agentCmdCore
    :: Submitting
    -> Wallet
    -> TokenId
    -> AgentCommand Ready result
    -> ClientM result
agentCmdCore sbmt wallet tokenId cmd = case cmd of
    Create key duration -> do
        createCommand sbmt wallet tokenId key duration
    Accept key pending -> acceptCommand sbmt wallet tokenId key pending
    Reject key pending reason ->
        rejectCommand sbmt wallet tokenId key pending reason
    Report key running duration url ->
        reportCommand sbmt wallet tokenId key running duration url

signAndSubmitAnUpdate
    :: (ToJSON ClientM old, ToJSON ClientM new, ToJSON ClientM res)
    => Submitting
    -> Wallet
    -> TokenId
    -> old
    -> new
    -> res
    -> ClientM (WithTxHash res)
signAndSubmitAnUpdate sbmt wallet tokenId testRun oldState newState = do
    WithTxHash txHash _ <- signAndSubmit sbmt wallet $ \address -> do
        key <- toJSON testRun
        oldValue <- toJSON oldState
        newValue <- toJSON newState
        requestUpdate address tokenId
            $ RequestUpdateBody{key, oldValue, newValue}
    pure $ WithTxHash txHash $ Just newState

createCommand
    :: Submitting
    -> Wallet
    -> TokenId
    -> TestRun
    -> Duration
    -> ClientM (WithTxHash (TestRunState PendingT))
createCommand sbmt wallet tokenId testRun duration = do
    let newState = Pending duration
    WithTxHash txHash _ <- signAndSubmit sbmt wallet $ \address -> do
        key <- toJSON testRun
        value <- toJSON newState
        requestInsert address tokenId
            $ RequestInsertBody{key, value}
    pure $ WithTxHash txHash $ Just newState

reportCommand
    :: Submitting
    -> Wallet
    -> TokenId
    -> TestRun
    -> TestRunState RunningT
    -> Duration
    -> URL
    -> ClientM (WithTxHash (TestRunState DoneT))
reportCommand sbmt wallet tokenId testRun testRunState duration url =
    signAndSubmitAnUpdate sbmt wallet tokenId testRun testRunState
        $ Finished testRunState duration url

rejectCommand
    :: Submitting
    -> Wallet
    -> TokenId
    -> TestRun
    -> TestRunState PendingT
    -> [Reason]
    -> ClientM (WithTxHash (TestRunState DoneT))
rejectCommand sbmt wallet tokenId testRun testRunState reason =
    signAndSubmitAnUpdate sbmt wallet tokenId testRun testRunState
        $ Rejected testRunState reason

acceptCommand
    :: Submitting
    -> Wallet
    -> TokenId
    -> TestRun
    -> TestRunState PendingT
    -> ClientM (WithTxHash (TestRunState RunningT))
acceptCommand sbmt wallet tokenId testRun testRunState =
    signAndSubmitAnUpdate sbmt wallet tokenId testRun testRunState
        $ Accepted testRunState
