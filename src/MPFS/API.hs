module MPFS.API
    ( requestInsertFromFacts
    , requestDeleteFromFacts
    , requestUpdateFromFacts
    , retractChangeFromFacts
    , updateTokenFromFacts
    , getToken
    , getTokenV2
    , getTokenFacts
    , submitRequestFromSignedTx
    , submitTransactionV2
    , RequestInsertBody (..)
    , RequestDeleteBody (..)
    , RequestUpdateBody (..)
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
    , SubmitRequest (..)
    , SubmitResponse (..)
    , TokenResponse
    , UpdateRequest
    , UpdateValueRequest
    )
import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types.Facts (UpdateFacts)
import Control.Monad (void)
import Core.Types.Basic (Address, RequestRefId, TokenId)
import Core.Types.Tx (SignedTx (..), TxHash (..), WithUnsignedTx)
import Data.Aeson (Value (..))
import Data.ByteString.Base16 qualified as Base16
import Data.Data (Proxy (..))
import Data.Text.Encoding qualified as Text
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
    , ReqBody
    , type (:>)
    )
import Servant.Client (ClientM, client)
import Text.JSON.Canonical
    ( JSValue (..)
    )

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

type GetTokenV2 =
    "tokens"
        :> Capture "tokenId" TokenId
        :> Get '[JSON] Value

type GetTokenFactsRead =
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

type SubmitTransactionV2 =
    "submit"
        :> ReqBody '[JSON] SubmitRequest
        :> Post '[JSON] SubmitResponse

type AwaitTransactionV2 =
    "tx"
        :> Capture "txId" TxHash
        :> QueryParam "timeout" Int
        :> Get '[JSON] NoContent

bootToken
    :: Address -> ClientM (WithUnsignedTx JSValue)
bootToken =
    bootTokenFromFacts status' bootFacts'
endToken
    :: Address -> TokenId -> ClientM (WithUnsignedTx JSValue)
endToken =
    endTokenFromFacts status' endFacts'

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
    -> [RequestRefId]
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

submitTransactionV2 :: SignedTx -> ClientM TxHash
submitTransactionV2 signed =
    submitResponseToTxHash
        <$> submitTransactionV2' (submitRequestFromSignedTx signed)

submitRequestFromSignedTx :: SignedTx -> SubmitRequest
submitRequestFromSignedTx (SignedTx signedTx) =
    SubmitRequest
        $ Hex
        $ case Base16.decode (Text.encodeUtf8 signedTx) of
            Right signedTxCbor -> signedTxCbor
            Left err ->
                error
                    $ "SignedTx is not hex-encoded CBOR: " <> err

submitResponseToTxHash :: SubmitResponse -> TxHash
submitResponseToTxHash (SubmitResponse (Hex txId)) =
    TxHash $ Text.decodeUtf8 $ Base16.encode txId

awaitTransactionV2 :: TxHash -> ClientM ()
awaitTransactionV2 txHash =
    void $ awaitTransactionV2' txHash (Just 1)

getTokenV2' :: TokenId -> ClientM Value
getTokenRead' :: TokenId -> ClientM TokenResponse
getTokenFacts' :: TokenId -> ClientM FactsResponse
getTokenRequests' :: TokenId -> ClientM RequestsResponse
submitTransactionV2' :: SubmitRequest -> ClientM SubmitResponse
awaitTransactionV2' :: TxHash -> Maybe Int -> ClientM NoContent

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

getTokenFacts' =
    client (Proxy :: Proxy GetTokenFactsRead)

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
        , mpfsRequestInsertFromFacts = requestInsertFromFacts
        , mpfsRequestDeleteFromFacts = requestDeleteFromFacts
        , mpfsRequestUpdateFromFacts = requestUpdateFromFacts
        , mpfsRetractChangeFromFacts = retractChangeFromFacts
        , mpfsUpdateTokenFromFacts = updateTokenFromFacts
        , mpfsGetToken = getToken
        , mpfsGetTokenFacts = getTokenFacts
        }

data MPFS m = MPFS
    { mpfsBootToken :: Address -> m (WithUnsignedTx JSValue)
    , mpfsEndToken :: Address -> TokenId -> m (WithUnsignedTx JSValue)
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
        -> [RequestRefId]
        -> m (WithUnsignedTx JSValue)
    , mpfsGetToken :: TokenId -> m JSValue
    , mpfsGetTokenFacts :: TokenId -> m JSValue
    }
