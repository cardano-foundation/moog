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
    ) where

import Core.Types
    ( Address
    , RequestRefId
    , SignedTx
    , TokenId
    , TxHash
    , WithUnsignedTx
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Data (Proxy (..))
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Traversable (for)
import Lib.JSON
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
            , "value" .= toAesonString v
            ]

instance FromJSON RequestInsertBody where
    parseJSON = withObject "RequestInsertBody" $ \o ->
        RequestInsertBody
            <$> (o .: "key" >>= fromAesonString)
            <*> (o .: "value" >>= fromAesonString)

data RequestDeleteBody = RequestDeleteBody
    { key :: JSValue
    , value :: JSValue
    }

instance ToJSON RequestDeleteBody where
    toJSON (RequestDeleteBody k v) =
        object
            [ "key" .= toAesonString k
            , "value" .= toAesonString v
            ]

instance FromJSON RequestDeleteBody where
    parseJSON = withObject "RequestDeleteBody" $ \o ->
        RequestDeleteBody
            <$> (o .: "key" >>= fromAesonString)
            <*> (o .: "value" >>= fromAesonString)

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
getTokenFacts tokenId = do
    aesonFacts <- getTokenFacts' tokenId
    pure
        $ fromMaybe JSNull
        $ case aesonFacts of
            Object obj -> do
                es <- for (Aeson.toList obj) $ \(key, value) -> do
                    key' <- parseMaybe fromAesonString $ toJSON key
                    value' <- parseMaybe fromAesonString value
                    pure (key', value')
                pure $ JSObject $ es >>= explode
            _ -> error "getTokenFacts: expected JSObject"

explode :: IsString a => (b, b) -> [(a, b)]
explode (key, value) =
    [ ("key", key)
    , ("value", value)
    ]

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
