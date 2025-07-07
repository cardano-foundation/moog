module Oracle.Token.Cli
    ( tokenCmdCore
    , TokenCommand (..)
    ) where

import Core.Types
    ( RequestRefId
    , TokenId
    , TxHash
    , Wallet
    , WithTxHash (WithTxHash)
    )
import Lib.JSON ()
import MPFS.API
    ( bootToken
    , endToken
    , getToken
    , updateToken
    )
import Servant.Client (ClientM)
import Submitting (Submitting, signAndSubmit)
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , JSValue (..)
    )

data TokenCommand a where
    GetToken :: TokenCommand JSValue
    BootToken :: TokenCommand (WithTxHash TokenId)
    UpdateToken
        :: {requests :: [RequestRefId]} -> TokenCommand TxHash
    EndToken :: TokenCommand TxHash

deriving instance Show (TokenCommand a)
deriving instance Eq (TokenCommand a)

tokenCmdCore
    :: Submitting -> Wallet -> Maybe TokenId -> TokenCommand a -> ClientM a
tokenCmdCore sbmt wallet (Just tk) cmd = do
    case cmd of
        GetToken -> getToken tk
        UpdateToken reqs -> do
            WithTxHash txHash _ <- signAndSubmit sbmt wallet $ \address ->
                updateToken address tk reqs
            pure txHash
        BootToken -> error "BootToken command requires no TokenId"
        EndToken -> do
            WithTxHash txHash _ <- signAndSubmit sbmt wallet $ \address -> endToken address tk
            pure txHash
tokenCmdCore sbmt wallet Nothing cmd = case cmd of
    BootToken -> do
        WithTxHash txHash jTokenId <- signAndSubmit sbmt wallet
            $ \address -> bootToken address
        case jTokenId of
            Just tkId -> case fromJSON tkId of
                Nothing -> error "BootToken failed, TokenId is not valid JSON"
                Just tokenId -> pure $ WithTxHash txHash (Just tokenId)
            _ -> error "BootToken failed, no TokenId returned"
    _ -> error "TokenId is required for this command"
