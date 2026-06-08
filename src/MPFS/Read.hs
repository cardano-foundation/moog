module MPFS.Read
    ( factEntriesToJSValue
    , factsResponseToJSValue
    , getTokenFromVerifiedRead
    , getTokenFactsFromVerifiedRead
    , requestJSONToRequestZoo
    , tokenResponsesToJSValue
    , verifyFactsResponseWith
    , verifyTokenResponsesWith
    ) where

import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( FactEntry (..)
    , FactsResponse (..)
    , RequestJSON (..)
    , RequestsResponse (..)
    , StatusResponse (..)
    , TokenResponse (..)
    , TokenStateJSON (..)
    , TxInJSON (..)
    , WitnessedRequest (..)
    , WitnessedTokenState (..)
    , WitnessedUtxo (..)
    )
import Cardano.MPFS.API.Types.Common (TokenIdJSON)
import Cardano.MPFS.Client.TrustedRoot (TrustedRoot (..))
import Cardano.MPFS.Client.Verify.Replay (VerifyError)
import Cardano.MPFS.Client.Verify.Read
    ( verifiedTokenRequests
    , verifiedTokenState
    , verifyTokenFacts
    , verifyTokenRequests
    , verifyTokenState
    , verifiedTokenFacts
    )
import Control.Monad.IO.Class (liftIO)
import Core.Types.Basic (Owner (..), RequestRefId (..), TokenId)
import Core.Types.Fact (Slot (..))
import Core.Types.Tx (Root (..))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as B8
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Lib.JSON.Canonical.Extra
    ( jsonToString
    , object
    , parseJSValue
    , (.=)
    )
import MPFS.Cage (liftEitherClientM, loadCageConfig)
import MPFS.End (tokenIdJSONFromTokenId)
import Oracle.Types
    ( RequestZoo
    , Token (..)
    , TokenState (..)
    )
import Servant.Client (ClientM)
import Text.JSON.Canonical (FromJSON (..), JSValue (..), ToJSON (..))

getTokenFactsFromVerifiedRead
    :: ClientM StatusResponse
    -> (TokenId -> ClientM FactsResponse)
    -> TokenId
    -> ClientM JSValue
getTokenFactsFromVerifiedRead getStatus getFacts tokenId = do
    StatusResponse{currentUtxoRoot} <- getStatus
    trustedRoot <-
        liftEitherClientM $ maybeToEither noUtxoRoot currentUtxoRoot
    facts <- getFacts tokenId
    verified <-
        liftEitherClientM
            $ verifyFactsResponseWith
                verifyFactsResponse
                (TrustedRoot trustedRoot)
                facts
    pure verified

getTokenFromVerifiedRead
    :: ClientM StatusResponse
    -> (TokenId -> ClientM TokenResponse)
    -> (TokenId -> ClientM RequestsResponse)
    -> TokenId
    -> ClientM JSValue
getTokenFromVerifiedRead getStatus getTokenState getRequests tokenId = do
    StatusResponse{currentUtxoRoot} <- getStatus
    trustedRoot <-
        liftEitherClientM $ maybeToEither noUtxoRoot currentUtxoRoot
    token <- liftEitherClientM $ tokenIdJSONFromTokenId tokenId
    cfg <- liftIO $ loadCageConfig BS.empty
    tokenState <- getTokenState tokenId
    requests <- getRequests tokenId
    liftEitherClientM
        $ verifyTokenResponsesWith
            (\trustedRoot' state' ->
                verifiedTokenState <$> verifyTokenState trustedRoot' state'
            )
            ( \token' trustedRoot' requests' ->
                verifiedTokenRequests
                    <$> verifyTokenRequests cfg token' trustedRoot' requests'
            )
            token
            (TrustedRoot trustedRoot)
            tokenState
            requests

verifyFactsResponseWith
    :: Show e
    => (TrustedRoot -> FactsResponse -> Either e FactsResponse)
    -> TrustedRoot
    -> FactsResponse
    -> Either String JSValue
verifyFactsResponseWith verify trustedRoot facts =
    factsResponseToJSValue <$> firstShow (verify trustedRoot facts)

verifyTokenResponsesWith
    :: Show e
    => (TrustedRoot -> TokenResponse -> Either e TokenResponse)
    -> (TokenIdJSON -> TrustedRoot -> RequestsResponse -> Either e RequestsResponse)
    -> TokenIdJSON
    -> TrustedRoot
    -> TokenResponse
    -> RequestsResponse
    -> Either String JSValue
verifyTokenResponsesWith verifyState verifyRequests token trustedRoot state requests = do
    verifiedState <- firstShow $ verifyState trustedRoot state
    verifiedRequests <- firstShow $ verifyRequests token trustedRoot requests
    tokenResponsesToJSValue verifiedState verifiedRequests

tokenResponsesToJSValue
    :: TokenResponse -> RequestsResponse -> Either String JSValue
tokenResponsesToJSValue
    TokenResponse{trState = state}
    RequestsResponse{rrRequests = requests} = do
        requestZoos <- traverse requestJSONToRequestZoo requests
        pure
            $ runIdentity
            $ toJSON
            $ Token
                { tokenRefId = txInRefId $ wuTxIn $ wtsUtxo state
                , tokenState = witnessedTokenStateToTokenState state
                , tokenRequests = Identity <$> requestZoos
                }

factsResponseToJSValue :: FactsResponse -> JSValue
factsResponseToJSValue FactsResponse{frsFacts} =
    factEntriesToJSValue frsFacts

factEntriesToJSValue :: [FactEntry] -> JSValue
factEntriesToJSValue facts =
    runIdentity $ object =<< traverse factEntryToPair facts

factEntryToPair :: FactEntry -> Identity (String, Identity JSValue)
factEntryToPair FactEntry{feKey = Hex key, feValue = Hex value} =
    pure
        ( B8.unpack key
        , object
            [ "value" .= B8.unpack value
            , "slot" .= Slot 0
            ]
        )

firstShow :: Show e => Either e a -> Either String a
firstShow =
    either (Left . show) Right

verifyFactsResponse
    :: TrustedRoot -> FactsResponse -> Either VerifyError FactsResponse
verifyFactsResponse trustedRoot facts =
    verifiedTokenFacts <$> verifyTokenFacts trustedRoot facts

requestJSONToRequestZoo :: WitnessedRequest -> Either String RequestZoo
requestJSONToRequestZoo WitnessedRequest{wrUtxo, wrRequest} =
    let refId = txInRefId $ wuTxIn wrUtxo
    in  requestJSONToJSValue refId wrRequest >>= parseRequestZoo

requestJSONToJSValue
    :: RequestRefId -> RequestJSON -> Either String JSValue
requestJSONToJSValue
    refId
    RequestJSON{rjOwner, rjKey = Hex key, rjOperation, rjValue} = do
        keyValue <- canonicalBytesToJSValue "request.key" (Hex key)
        operationFields <- requestOperationFields rjOperation rjValue
        pure
            $ runIdentity
            $ object
                [ "outputRefId" .= refId
                , "owner" .= Owner (T.unpack rjOwner)
                ,
                    ( "change"
                    , object
                        $ ("key" .= jsonToString keyValue)
                            : operationFields
                    )
                ]

requestOperationFields
    :: T.Text -> Maybe Hex -> Either String [(String, Identity JSValue)]
requestOperationFields "insert" maybeValue = do
    value <- requiredValue "insert" maybeValue
    pure
        [ "type" .= ("insert" :: String)
        , "newValue" .= jsonToString value
        ]
requestOperationFields "delete" maybeValue = do
    value <- optionalValue maybeValue
    pure
        [ "type" .= ("delete" :: String)
        , "oldValue" .= jsonToString value
        ]
requestOperationFields "update" maybeValue = do
    value <- optionalValue maybeValue
    pure
        [ "type" .= ("update" :: String)
        , "oldValue" .= jsonToString JSNull
        , "newValue" .= jsonToString value
        ]
requestOperationFields operation _ =
    Left $ "unknown request operation: " <> T.unpack operation

requiredValue :: String -> Maybe Hex -> Either String JSValue
requiredValue operation =
    maybe
        (Left $ operation <> " request is missing value")
        (canonicalBytesToJSValue $ "request." <> operation <> ".value")

optionalValue :: Maybe Hex -> Either String JSValue
optionalValue Nothing = Right JSNull
optionalValue (Just hexValue) =
    canonicalBytesToJSValue "request.value" hexValue

canonicalBytesToJSValue :: String -> Hex -> Either String JSValue
canonicalBytesToJSValue field (Hex bytes) =
    maybeToEither
        (field <> " is not canonical JSON")
        (parseJSValue bytes :: Maybe JSValue)

parseRequestZoo :: JSValue -> Either String RequestZoo
parseRequestZoo value =
    maybeToEither
        "verified request JSON did not match moog RequestZoo"
        (fromJSON value)

witnessedTokenStateToTokenState :: WitnessedTokenState -> TokenState
witnessedTokenStateToTokenState
    WitnessedTokenState{wtsState = TokenStateJSON{owner, root = Hex root}} =
        TokenState
            { tokenRoot = Root $ hexString root
            , tokenOwner = Owner $ T.unpack owner
            }

txInRefId :: TxInJSON -> RequestRefId
txInRefId TxInJSON{tjTxId = Hex txId, tjTxIx} =
    RequestRefId
        $ T.decodeUtf8 (Base16.encode txId)
            <> "#"
            <> T.pack (show tjTxIx)

hexString :: BS.ByteString -> String
hexString =
    B8.unpack . Base16.encode

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e =
    maybe (Left e) Right

noUtxoRoot :: String
noUtxoRoot =
    "GET /status returned no utxo_root; MPFS indexer is not ready"
