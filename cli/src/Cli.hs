module Cli
    ( cmd
    , Command (..)
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Types
    ( RequestRefId
    , TokenId
    , TxHash
    , Wallet
    , WithTxHash (..)
    )
import MPFS.API (getTokenFacts, retractChange)
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Servant.Client (ClientM)
import Submitting (Submitting, signAndSubmit)
import Text.JSON.Canonical (JSValue)
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
    RequesterCommand :: RequesterCommand a -> Command a
    OracleCommand :: OracleCommand a -> Command a
    AgentCommand :: AgentCommand NotReady a -> Command a
    RetractRequest
        :: { outputReference :: RequestRefId
           }
        -> Command TxHash
    GetFacts :: Command JSValue
    Wallet :: WalletCommand a -> Command a

deriving instance Show (Command a)
deriving instance Eq (Command a)

cmd
    :: Submitting
    -> Either FilePath Wallet
    -> Maybe TokenId
    -> Command a
    -> ClientM a
cmd sbmt (Right wallet) (Just tokenId) command =
    case command of
        RequesterCommand requesterCommand ->
            requesterCmd sbmt wallet tokenId requesterCommand
        OracleCommand oracleCommand ->
            oracleCmd sbmt wallet (Just tokenId) oracleCommand
        AgentCommand agentCommand -> agentCmd sbmt wallet tokenId agentCommand
        GetFacts -> getTokenFacts tokenId
        RetractRequest refId -> fmap txHash $ signAndSubmit sbmt wallet $ \address ->
            retractChange address refId
        Wallet walletCommand -> liftIO $ walletCmd (Right wallet) walletCommand
cmd sbmt (Right wallet) Nothing command =
    case command of
        RetractRequest refId -> fmap txHash $ signAndSubmit sbmt wallet $ \address ->
            retractChange address refId
        Wallet walletCommand -> liftIO $ walletCmd (Right wallet) walletCommand
        OracleCommand oracleCommand -> oracleCmd sbmt wallet Nothing oracleCommand
        _ -> error "TokenId is required for this command"
cmd _iftw mwf@(Left _) (Just tokenId) command =
    case command of
        GetFacts -> getTokenFacts tokenId
        Wallet walletCommand -> liftIO $ walletCmd mwf walletCommand
        _ -> error "Wallet is required for this command"
cmd _iftw mwf@(Left _) Nothing command =
    case command of
        Wallet walletCommand -> liftIO $ walletCmd mwf walletCommand
        _ -> error "Wallet and TokenId are required for this command"
