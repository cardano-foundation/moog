module MPFS.API
    ( tokenApi
    , TokenAPI
    , requestInsert
    , requestDelete
    , requestUpdate
    , retractChange
    , updateToken
    , getToken
    , getTokenFacts
    , submitTransaction
    , waitNBlocks
    , RequestInsertBody (..)
    , RequestDeleteBody (..)
    , RequestUpdateBody (..)
    , getTransaction
    , bootToken
    , endToken
    , MPFS (..)
    , mpfsClient
    ) where

import Core.Types.Basic (Address, RequestRefId, TokenId)
import Core.Types.Tx (SignedTx, TxHash, WithUnsignedTx)
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Data (Proxy (..))
import Lib.JSON.Canonical.Extra
    ( fromAesonString
    , fromAesonThrow
    , toAesonString
    )
import Servant.API
    ( Capture
    , Get
    , JSON
    , Post
    , QueryParam'
    , QueryParams
    , ReqBody
    , Required
    , (:<|>) (..)
    , type (:>)
    )
import Servant.Client (ClientM, client)
import Text.JSON.Canonical
    ( JSValue (..)
    )

data RequestInsertBody = RequestInsertBody
    { key :: JSValue
    , value :: JSValue
    }

instance ToJSON RequestInsertBody where
    toJSON (RequestInsertBody k v) =
        object
            [ "key" .= toAesonString k
            , "newValue" .= toAesonString v
            ]

instance FromJSON RequestInsertBody where
    parseJSON = withObject "RequestInsertBody" $ \o ->
        RequestInsertBody
            <$> (o .: "key" >>= fromAesonString)
            <*> (o .: "newValue" >>= fromAesonString)

data RequestDeleteBody = RequestDeleteBody
    { key :: JSValue
    , value :: JSValue
    }

instance ToJSON RequestDeleteBody where
    toJSON (RequestDeleteBody k v) =
        object
            [ "key" .= toAesonString k
            , "oldValue" .= toAesonString v
            ]

instance FromJSON RequestDeleteBody where
    parseJSON = withObject "RequestDeleteBody" $ \o ->
        RequestDeleteBody
            <$> (o .: "key" >>= fromAesonString)
            <*> (o .: "oldValue" >>= fromAesonString)

data RequestUpdateBody = RequestUpdateBody
    { key :: JSValue
    , oldValue :: JSValue
    , newValue :: JSValue
    }

instance ToJSON RequestUpdateBody where
    toJSON (RequestUpdateBody k old new) =
        object
            [ "key" .= toAesonString k
            , "oldValue" .= toAesonString old
            , "newValue" .= toAesonString new
            ]

instance FromJSON RequestUpdateBody where
    parseJSON = withObject "RequestUpdateBody" $ \o ->
        RequestUpdateBody
            <$> (o .: "key" >>= fromAesonString)
            <*> (o .: "oldValue" >>= fromAesonString)
            <*> (o .: "newValue" >>= fromAesonString)
type BootToken =
    "transaction"
        :> Capture "address" Address
        :> "boot-token"
        :> Get '[JSON] (WithUnsignedTx Value)

type EndToken =
    "transaction"
        :> Capture "address" Address
        :> "end-token"
        :> Capture "tokenId" TokenId
        :> Get '[JSON] (WithUnsignedTx Value)

type RequestInsert =
    "transaction"
        :> Capture "address" Address
        :> "request-insert"
        :> Capture "tokenId" TokenId
        :> ReqBody '[JSON] RequestInsertBody
        :> Post '[JSON] (WithUnsignedTx Value)

type RequestDelete =
    "transaction"
        :> Capture "address" Address
        :> "request-delete"
        :> Capture "tokenId" TokenId
        :> ReqBody '[JSON] RequestDeleteBody
        :> Post '[JSON] (WithUnsignedTx Value)

type RequestUpdate =
    "transaction"
        :> Capture "address" Address
        :> "request-update"
        :> Capture "tokenId" TokenId
        :> ReqBody '[JSON] RequestUpdateBody
        :> Post '[JSON] (WithUnsignedTx Value)

type RetractChange =
    "transaction"
        :> Capture "address" Address
        :> "retract-change"
        :> Capture "requestId" RequestRefId
        :> Get '[JSON] (WithUnsignedTx Value)

type UpdateToken =
    "transaction"
        :> Capture "address" Address
        :> "update-token"
        :> Capture "tokenId" TokenId
        :> QueryParams "request" RequestRefId
        :> Get '[JSON] (WithUnsignedTx Value)

type GetToken =
    "token"
        :> Capture "tokenId" TokenId
        :> Get '[JSON] Value

type GetTokenFacts =
    "token"
        :> Capture "tokenId" TokenId
        :> "facts"
        :> Get '[JSON] Value

type SubmitTransaction =
    "transaction"
        :> ReqBody '[JSON] SignedTx
        :> Post '[JSON] TxHash

type WaitNBlocks =
    "wait"
        :> Capture "n" Int
        :> Get '[JSON] Value

type GetTransaction =
    "transaction"
        :> QueryParam' '[Required] "txHash" TxHash
        :> Get '[JSON] Value

type TokenAPI =
    BootToken
        :<|> EndToken
        :<|> RequestInsert
        :<|> RequestDelete
        :<|> RequestUpdate
        :<|> RetractChange
        :<|> UpdateToken
        :<|> GetToken
        :<|> GetTokenFacts
        :<|> SubmitTransaction
        :<|> WaitNBlocks
        :<|> GetTransaction

tokenApi :: Proxy TokenAPI
tokenApi = Proxy

bootToken
    :: Address -> ClientM (WithUnsignedTx JSValue)
bootToken address =
    fmap fromAesonThrow <$> bootToken' address
endToken
    :: Address -> TokenId -> ClientM (WithUnsignedTx JSValue)
endToken address tokenId =
    fmap fromAesonThrow <$> endToken' address tokenId
requestInsert
    :: Address
    -> TokenId
    -> RequestInsertBody
    -> ClientM (WithUnsignedTx JSValue)
requestInsert address tokenId body =
    fmap fromAesonThrow <$> requestInsert' address tokenId body
requestDelete
    :: Address
    -> TokenId
    -> RequestDeleteBody
    -> ClientM (WithUnsignedTx JSValue)
requestDelete address tokenId body =
    fmap fromAesonThrow <$> requestDelete' address tokenId body
requestUpdate
    :: Address
    -> TokenId
    -> RequestUpdateBody
    -> ClientM (WithUnsignedTx JSValue)
requestUpdate address tokenId body =
    fmap fromAesonThrow <$> requestUpdate' address tokenId body
retractChange
    :: Address -> RequestRefId -> ClientM (WithUnsignedTx JSValue)
retractChange address requestId =
    fmap fromAesonThrow <$> retractChange' address requestId
updateToken
    :: Address
    -> TokenId
    -> [RequestRefId]
    -> ClientM (WithUnsignedTx JSValue)
updateToken address tokenId requests =
    fmap fromAesonThrow <$> updateToken' address tokenId requests

getToken :: TokenId -> ClientM JSValue
getToken tokenId = fromAesonThrow <$> getToken' tokenId

getTokenFacts :: TokenId -> ClientM JSValue
getTokenFacts tokenId = fromAesonThrow <$> getTokenFacts' tokenId

submitTransaction :: SignedTx -> ClientM TxHash
submitTransaction = submitTransaction'
waitNBlocks :: Int -> ClientM JSValue
waitNBlocks n = fromAesonThrow <$> waitNBlocks' n
getTransaction :: TxHash -> ClientM JSValue
getTransaction txHash = fromAesonThrow <$> getTransaction' txHash

requestInsert'
    :: Address
    -> TokenId
    -> RequestInsertBody
    -> ClientM (WithUnsignedTx Value)
requestDelete'
    :: Address
    -> TokenId
    -> RequestDeleteBody
    -> ClientM (WithUnsignedTx Value)
requestUpdate'
    :: Address
    -> TokenId
    -> RequestUpdateBody
    -> ClientM (WithUnsignedTx Value)
retractChange'
    :: Address -> RequestRefId -> ClientM (WithUnsignedTx Value)
updateToken'
    :: Address -> TokenId -> [RequestRefId] -> ClientM (WithUnsignedTx Value)
getToken' :: TokenId -> ClientM Value
getTokenFacts' :: TokenId -> ClientM Value
submitTransaction' :: SignedTx -> ClientM TxHash
waitNBlocks' :: Int -> ClientM Value
getTransaction' :: TxHash -> ClientM Value
bootToken'
    :: Address -> ClientM (WithUnsignedTx Value)
endToken'
    :: Address -> TokenId -> ClientM (WithUnsignedTx Value)
bootToken'
    :<|> endToken'
    :<|> requestInsert'
    :<|> requestDelete'
    :<|> requestUpdate'
    :<|> retractChange'
    :<|> updateToken'
    :<|> getToken'
    :<|> getTokenFacts'
    :<|> submitTransaction'
    :<|> waitNBlocks'
    :<|> getTransaction' =
        client tokenApi

mpfsClient :: MPFS ClientM
mpfsClient =
    MPFS
        { mpfsBootToken = bootToken
        , mpfsEndToken = endToken
        , mpfsRequestInsert = requestInsert
        , mpfsRequestDelete = requestDelete
        , mpfsRequestUpdate = requestUpdate
        , mpfsRetractChange = retractChange
        , mpfsUpdateToken = updateToken
        , mpfsGetToken = getToken
        , mpfsGetTokenFacts = getTokenFacts
        , mpfsSubmitTransaction = submitTransaction
        , mpfsWaitNBlocks = waitNBlocks
        , mpfsGetTransaction = getTransaction
        }

data MPFS m = MPFS
    { mpfsBootToken :: Address -> m (WithUnsignedTx JSValue)
    , mpfsEndToken :: Address -> TokenId -> m (WithUnsignedTx JSValue)
    , mpfsRequestInsert
        :: Address
        -> TokenId
        -> RequestInsertBody
        -> m (WithUnsignedTx JSValue)
    , mpfsRequestDelete
        :: Address
        -> TokenId
        -> RequestDeleteBody
        -> m (WithUnsignedTx JSValue)
    , mpfsRequestUpdate
        :: Address
        -> TokenId
        -> RequestUpdateBody
        -> m (WithUnsignedTx JSValue)
    , mpfsRetractChange
        :: Address
        -> RequestRefId
        -> m (WithUnsignedTx JSValue)
    , mpfsUpdateToken
        :: Address
        -> TokenId
        -> [RequestRefId]
        -> m (WithUnsignedTx JSValue)
    , mpfsGetToken :: TokenId -> m JSValue
    , mpfsGetTokenFacts :: TokenId -> m JSValue
    , mpfsSubmitTransaction :: SignedTx -> m TxHash
    , mpfsWaitNBlocks :: Int -> m JSValue
    , mpfsGetTransaction :: TxHash -> m JSValue
    }
