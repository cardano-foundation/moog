module MPFS.API
    ( tokenApi
    , TokenAPI
    , SubmitV2Body (..)
    , requestInsert
    , requestInsertFromFacts
    , requestDelete
    , requestDeleteFromFacts
    , requestUpdate
    , requestUpdateFromFacts
    , retractChange
    , retractChangeFromFacts
    , updateToken
    , updateTokenFromFacts
    , getToken
    , getTokenV2
    , getTokenFacts
    , submitTransaction
    , submitTransactionV2
    , waitNBlocks
    , RequestInsertBody (..)
    , RequestDeleteBody (..)
    , RequestUpdateBody (..)
    , getTransaction
    , awaitTransactionV2
    , bootToken
    , endToken
    , MPFS (..)
    , mpfsClient
    ) where

import Cardano.MPFS.API.Types
    ( BootFacts
    , BootRequest
    , DeleteRequest
    , EndFacts
    , EndRequest
    , FactsResponse
    , InsertRequest
    , RequestDeleteFacts
    , RequestInsertFacts
    , RequestUpdateFacts
    , RequestsResponse
    , RetractFacts
    , RetractRequest
    , StatusResponse
    , TokenResponse
    , UpdateRequest
    , UpdateValueRequest
    )
import Cardano.MPFS.API.Types.Facts (UpdateFacts)
import Control.Monad (void)
import Core.Types.Basic (Address, RequestRefId, TokenId)
import Core.Types.Tx (SignedTx (..), TxHash (..), WithUnsignedTx)
import Data.Aeson
    ( ToJSON (..)
    , Value (..)
    , object
    , (.=)
    )
import Data.Data (Proxy (..))
import Data.Text (Text)
import Lib.JSON.Canonical.Extra (fromAesonThrow)
import MPFS.Boot (bootTokenFromFacts)
import MPFS.End (endTokenFromFacts)
import MPFS.Read
    ( getTokenFactsFromVerifiedRead
    , getTokenFromVerifiedRead
    )
import MPFS.Request
    ( RequestDeleteBody (..)
    , RequestInsertBody (..)
    , RequestUpdateBody (..)
    )
import MPFS.Request qualified as Request
import MPFS.Retract qualified as Retract
import MPFS.Update qualified as Update
import Servant.API
    ( Capture
    , Get
    , JSON
    , NoContent
    , Post
    , QueryParam
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

newtype SubmitV2Body = SubmitV2Body SignedTx

instance ToJSON SubmitV2Body where
    toJSON (SubmitV2Body (SignedTx signedTx)) =
        object ["tx" .= signedTx]

type Status =
    "status"
        :> Get '[JSON] StatusResponse

type BootFactsEndpoint =
    "facts"
        :> "boot"
        :> ReqBody '[JSON] BootRequest
        :> Post '[JSON] BootFacts

type EndFactsEndpoint =
    "facts"
        :> "end"
        :> ReqBody '[JSON] EndRequest
        :> Post '[JSON] EndFacts

type RequestInsertFactsEndpoint =
    "facts"
        :> "request"
        :> "insert"
        :> ReqBody '[JSON] InsertRequest
        :> Post '[JSON] RequestInsertFacts

type RequestDeleteFactsEndpoint =
    "facts"
        :> "request"
        :> "delete"
        :> ReqBody '[JSON] DeleteRequest
        :> Post '[JSON] RequestDeleteFacts

type RequestUpdateFactsEndpoint =
    "facts"
        :> "request"
        :> "update"
        :> ReqBody '[JSON] UpdateValueRequest
        :> Post '[JSON] RequestUpdateFacts

type UpdateFactsEndpoint =
    "facts"
        :> "update"
        :> ReqBody '[JSON] UpdateRequest
        :> Post '[JSON] UpdateFacts

type RetractFactsEndpoint =
    "facts"
        :> "retract"
        :> ReqBody '[JSON] RetractRequest
        :> Post '[JSON] RetractFacts

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

type GetTokenV2 =
    "tokens"
        :> Capture "tokenId" TokenId
        :> Get '[JSON] Value

type GetTokenFacts =
    "tokens"
        :> Capture "tokenId" TokenId
        :> "facts"
        :> Get '[JSON] FactsResponse

type GetTokenRead =
    "tokens"
        :> Capture "tokenId" TokenId
        :> Get '[JSON] TokenResponse

type GetTokenRequests =
    "tokens"
        :> Capture "tokenId" TokenId
        :> "requests"
        :> Get '[JSON] RequestsResponse

type SubmitTransaction =
    "transaction"
        :> ReqBody '[JSON] SignedTx
        :> Post '[JSON] TxHash

type SubmitTransactionV2 =
    "tx"
        :> "submit"
        :> ReqBody '[JSON] SubmitV2Body
        :> Post '[JSON] Text

type WaitNBlocks =
    "wait"
        :> Capture "n" Int
        :> Get '[JSON] Value

type GetTransaction =
    "transaction"
        :> QueryParam' '[Required] "txHash" TxHash
        :> Get '[JSON] Value

type AwaitTransactionV2 =
    "tx"
        :> Capture "txId" TxHash
        :> QueryParam "timeout" Int
        :> Get '[JSON] NoContent

type TokenAPI =
    EndToken
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
bootToken =
    bootTokenFromFacts status' bootFacts'
endToken
    :: Address -> TokenId -> ClientM (WithUnsignedTx JSValue)
endToken =
    endTokenFromFacts status' endFacts'
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

requestInsertFromFacts
    :: Address
    -> TokenId
    -> RequestInsertBody
    -> ClientM (WithUnsignedTx JSValue)
requestInsertFromFacts =
    Request.requestInsertFromFacts status' requestInsertFacts'

requestDeleteFromFacts
    :: Address
    -> TokenId
    -> RequestDeleteBody
    -> ClientM (WithUnsignedTx JSValue)
requestDeleteFromFacts =
    Request.requestDeleteFromFacts status' requestDeleteFacts'

requestUpdateFromFacts
    :: Address
    -> TokenId
    -> RequestUpdateBody
    -> ClientM (WithUnsignedTx JSValue)
requestUpdateFromFacts =
    Request.requestUpdateFromFacts status' requestUpdateFacts'

updateTokenFromFacts
    :: Address
    -> TokenId
    -> ClientM (WithUnsignedTx JSValue)
updateTokenFromFacts =
    Update.updateTokenFromFacts status' updateFacts'

retractChangeFromFacts
    :: Address -> RequestRefId -> ClientM (WithUnsignedTx JSValue)
retractChangeFromFacts =
    Retract.retractChangeFromFacts status' retractFacts'

getToken :: TokenId -> ClientM JSValue
getToken =
    getTokenFromVerifiedRead status' getTokenRead' getTokenRequests'

getTokenV2 :: TokenId -> ClientM JSValue
getTokenV2 tokenId = fromAesonThrow <$> getTokenV2' tokenId

getTokenFacts :: TokenId -> ClientM JSValue
getTokenFacts =
    getTokenFactsFromVerifiedRead status' getTokenFacts'

submitTransaction :: SignedTx -> ClientM TxHash
submitTransaction = submitTransaction'

submitTransactionV2 :: SignedTx -> ClientM TxHash
submitTransactionV2 signed =
    TxHash <$> submitTransactionV2' (SubmitV2Body signed)

waitNBlocks :: Int -> ClientM JSValue
waitNBlocks n = fromAesonThrow <$> waitNBlocks' n
getTransaction :: TxHash -> ClientM JSValue
getTransaction txHash = fromAesonThrow <$> getTransaction' txHash

awaitTransactionV2 :: TxHash -> ClientM ()
awaitTransactionV2 txHash =
    void $ awaitTransactionV2' txHash (Just 1)

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
_getToken' :: TokenId -> ClientM Value
getTokenV2' :: TokenId -> ClientM Value
getTokenRead' :: TokenId -> ClientM TokenResponse
getTokenFacts' :: TokenId -> ClientM FactsResponse
getTokenRequests' :: TokenId -> ClientM RequestsResponse
submitTransaction' :: SignedTx -> ClientM TxHash
submitTransactionV2' :: SubmitV2Body -> ClientM Text
waitNBlocks' :: Int -> ClientM Value
getTransaction' :: TxHash -> ClientM Value
awaitTransactionV2' :: TxHash -> Maybe Int -> ClientM NoContent
_endToken'
    :: Address -> TokenId -> ClientM (WithUnsignedTx Value)
_endToken'
    :<|> requestInsert'
    :<|> requestDelete'
    :<|> requestUpdate'
    :<|> retractChange'
    :<|> updateToken'
    :<|> _getToken'
    :<|> getTokenFacts'
    :<|> submitTransaction'
    :<|> waitNBlocks'
    :<|> getTransaction' =
        client tokenApi

status' :: ClientM StatusResponse
status' =
    client (Proxy :: Proxy Status)

bootFacts' :: BootRequest -> ClientM BootFacts
bootFacts' =
    client (Proxy :: Proxy BootFactsEndpoint)

endFacts' :: EndRequest -> ClientM EndFacts
endFacts' =
    client (Proxy :: Proxy EndFactsEndpoint)

requestInsertFacts' :: InsertRequest -> ClientM RequestInsertFacts
requestInsertFacts' =
    client (Proxy :: Proxy RequestInsertFactsEndpoint)

requestDeleteFacts' :: DeleteRequest -> ClientM RequestDeleteFacts
requestDeleteFacts' =
    client (Proxy :: Proxy RequestDeleteFactsEndpoint)

requestUpdateFacts' :: UpdateValueRequest -> ClientM RequestUpdateFacts
requestUpdateFacts' =
    client (Proxy :: Proxy RequestUpdateFactsEndpoint)

updateFacts' :: UpdateRequest -> ClientM UpdateFacts
updateFacts' =
    client (Proxy :: Proxy UpdateFactsEndpoint)

retractFacts' :: RetractRequest -> ClientM RetractFacts
retractFacts' =
    client (Proxy :: Proxy RetractFactsEndpoint)

getTokenV2' =
    client (Proxy :: Proxy GetTokenV2)

getTokenRead' =
    client (Proxy :: Proxy GetTokenRead)

getTokenRequests' =
    client (Proxy :: Proxy GetTokenRequests)

submitTransactionV2' =
    client (Proxy :: Proxy SubmitTransactionV2)

awaitTransactionV2' =
    client (Proxy :: Proxy AwaitTransactionV2)

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
        , mpfsRequestInsertFromFacts = requestInsertFromFacts
        , mpfsRequestDeleteFromFacts = requestDeleteFromFacts
        , mpfsRequestUpdateFromFacts = requestUpdateFromFacts
        , mpfsRetractChangeFromFacts = retractChangeFromFacts
        , mpfsUpdateTokenFromFacts = updateTokenFromFacts
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
    , mpfsRequestInsertFromFacts
        :: Address
        -> TokenId
        -> RequestInsertBody
        -> m (WithUnsignedTx JSValue)
    , mpfsRequestDeleteFromFacts
        :: Address
        -> TokenId
        -> RequestDeleteBody
        -> m (WithUnsignedTx JSValue)
    , mpfsRequestUpdateFromFacts
        :: Address
        -> TokenId
        -> RequestUpdateBody
        -> m (WithUnsignedTx JSValue)
    , mpfsRetractChangeFromFacts
        :: Address
        -> RequestRefId
        -> m (WithUnsignedTx JSValue)
    , mpfsUpdateTokenFromFacts
        :: Address
        -> TokenId
        -> m (WithUnsignedTx JSValue)
    , mpfsGetToken :: TokenId -> m JSValue
    , mpfsGetTokenFacts :: TokenId -> m JSValue
    , mpfsSubmitTransaction :: SignedTx -> m TxHash
    , mpfsWaitNBlocks :: Int -> m JSValue
    , mpfsGetTransaction :: TxHash -> m JSValue
    }
