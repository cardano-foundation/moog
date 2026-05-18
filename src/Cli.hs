module Cli
    ( cmd
    , Command (..)
    , WithValidation (..)
    , TokenInfoFailure (..)
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Core.Context (askConfig, askMpfs, askValidation, withContext)
import Core.Types.Basic (RequestRefId, TokenId)
import Core.Types.MPFS (MPFSClient (..))
import Core.Types.Tx (TxHash, WithTxHash (..))
import Core.Types.Wallet (Wallet)
import Data.Functor.Identity (Identity (..))
import Effects (mkEffects)
import Facts (FactsSelection, factsCmd)
import GitHub (Auth)
import Lib.JSON.Canonical.Extra
import Lib.SSH.Private
    ( SSHClient
    , Selection
    , WithSelector (..)
    , sshKeySelectors
    )
import MPFS.API
    ( MPFS (..)
    , mpfsClient
    , retractChange
    )
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Oracle.Config.Types (ProtocolFailure)
import Oracle.Types
    ( Token (..)
    , TokenState (..)
    , fmapMToken
    )
import Oracle.Validate.Failure (RequestValidationFailure)
import Oracle.Validate.Request (validateRequest)
import Oracle.Validate.Types
    ( AValidationResult
    , ValidationResult
    , liftMaybe
    , runValidate
    )
import Submitting (Submission (..))
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))
import User.Agent.Cli
    ( AgentCommand (..)
    , IsReady (NotReady)
    , agentCmd
    )
import User.Requester.Cli
    ( RequesterCommand
    , requesterCmd
    )
import Wallet.Cli (WalletCommand, walletCmd)

data Command a where
    RequesterCommand
        :: Auth -> MPFSClient -> RequesterCommand a -> Command a
    OracleCommand :: Auth -> MPFSClient -> OracleCommand a -> Command a
    AgentCommand
        :: Auth -> MPFSClient -> AgentCommand NotReady a -> Command a
    RetractRequest
        :: MPFSClient -> Wallet -> RequestRefId -> Command TxHash
    GetFacts
        :: MPFSClient
        -> TokenId
        -> FactsSelection a
        -> Command a
    Wallet :: WalletCommand a -> Command a
    GetToken
        :: Auth
        -> MPFSClient
        -> TokenId
        -> Command
            (AValidationResult TokenInfoFailure (Token WithValidation))
    SSHSelectors :: SSHClient 'WithoutSelector -> Command [Selection]

deriving instance Show (Token WithValidation)
deriving instance Eq (Token WithValidation)

data SetupError = TokenNotSpecified
    deriving (Show, Eq)

cmd :: Command a -> IO a
cmd = \case
    RequesterCommand
        auth
        MPFSClient{runMPFS, submitTx}
        requesterCommand -> do
            runMPFS
                $ withContext
                    mpfsClient
                    (mkEffects auth)
                    submitTx
                $ requesterCmd requesterCommand
    OracleCommand
        auth
        MPFSClient{runMPFS, submitTx}
        oracleCommand -> do
            runMPFS
                $ withContext
                    mpfsClient
                    (mkEffects auth)
                    submitTx
                $ oracleCmd oracleCommand
    AgentCommand
        auth
        MPFSClient{runMPFS, submitTx}
        agentCommand -> do
            runMPFS
                $ withContext
                    mpfsClient
                    (mkEffects auth)
                    submitTx
                $ agentCmd agentCommand
    RetractRequest
        MPFSClient{runMPFS, submitTx}
        wallet
        refId -> do
            let Submission submit = submitTx wallet
            runMPFS
                $ fmap txHash
                $ submit
                $ \address ->
                    retractChange address refId
    GetFacts MPFSClient{runMPFS} tokenId factsCommand -> do
        let validation =
                mkEffects
                    (error "shouldn't need this...")
                    mpfsClient
                    $ Just tokenId
        runMPFS
            $ factsCmd
                validation
                factsCommand
    Wallet walletCommand -> liftIO $ walletCmd walletCommand
    GetToken
        auth
        MPFSClient{runMPFS, submitTx}
        tk -> do
            runMPFS
                $ withContext
                    mpfsClient
                    (mkEffects auth)
                    submitTx
                $ do
                    validation <- askValidation $ Just tk
                    mconfig <- askConfig tk
                    mpfs <- askMpfs
                    lift $ runValidate $ do
                        mpendings <- lift $ fromJSON <$> mpfsGetToken mpfs tk
                        token <- liftMaybe (TokenInfoTokenNotParsable tk) mpendings
                        let oracle = tokenOwner $ tokenState token
                            f (Identity req) = do
                                r <-
                                    runValidate
                                        $ validateRequest oracle mconfig validation req
                                pure $ WithValidation r req
                        lift $ fmapMToken f token
    SSHSelectors sshClient -> sshKeySelectors sshClient

data TokenInfoFailure
    = TokenInfoTokenNotParsable TokenId
    | TokenInfoProtocolFailure ProtocolFailure
    deriving (Show, Eq)

instance Monad m => ToJSON m TokenInfoFailure where
    toJSON (TokenInfoTokenNotParsable tk) =
        object ["tokenNotParsable" .= tk]
    toJSON (TokenInfoProtocolFailure protocolFailure) =
        object ["protocolFailure" .= protocolFailure]

data WithValidation x = WithValidation
    { validation :: ValidationResult RequestValidationFailure
    , request :: x
    }
    deriving (Show, Eq)

instance (Monad m, ToJSON m x) => ToJSON m (WithValidation x) where
    toJSON (WithValidation mvalidation request) =
        object
            [ "validation" .= mvalidation
            , "request" .= request
            ]
