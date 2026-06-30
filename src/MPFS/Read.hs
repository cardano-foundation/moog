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

import Cardano.Ledger.Api (ConwayEra, eraProtVerLow)
import Cardano.Ledger.Api.Scripts.Data
    ( Data (..)
    , Datum (..)
    , binaryDataToData
    )
import Cardano.Ledger.Api.Tx.Out (TxOut, datumTxOutL)
import Cardano.Ledger.Binary (decodeFull)
import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( FactEntry (..)
    , FactsResponse (..)
    , RequestsResponse (..)
    , StatusResponse (..)
    , TokenResponse (..)
    , TxInJSON (..)
    , WitnessedRequest (..)
    , WitnessedTokenState (..)
    , WitnessedUtxo (..)
    )
import Cardano.MPFS.API.Types.Common (TokenIdJSON)
import Cardano.MPFS.Cage.Types
    ( CageDatum (..)
    , OnChainOperation (..)
    , OnChainRequest (..)
    , OnChainRoot (..)
    , OnChainTokenState (..)
    )
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
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Core.Types.Basic (Owner (..), RequestRefId (..), TokenId)
import Core.Types.Fact (Slot (..))
import Core.Types.Tx (Root (..))
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Lib.JSON.Canonical.Extra
    ( jsonToString
    , object
    , parseJSValue
    , (.=)
    )
import MPFS.Cage (awaitStatus, liftEitherClientM, loadCageConfig)
import MPFS.End (tokenIdJSONFromTokenId)
import Oracle.Types
    ( RequestZoo
    , Token (..)
    , TokenState (..)
    )
import PlutusTx.Builtins (fromBuiltin)
import PlutusTx.Builtins.Internal (BuiltinData (..))
import PlutusTx.IsData.Class (fromBuiltinData)
import Servant.Client (ClientM)
import Text.JSON.Canonical (FromJSON (..), JSValue (..), ToJSON (..))

getTokenFactsFromVerifiedRead
    :: ClientM StatusResponse
    -> (TokenId -> ClientM FactsResponse)
    -> TokenId
    -> ClientM JSValue
getTokenFactsFromVerifiedRead getStatus getFacts tokenId = do
    StatusResponse{currentUtxoRoot} <- awaitStatus getStatus
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
        tokenState <- witnessedTokenStateToTokenState state
        pure
            $ runIdentity
            $ toJSON
            $ Token
                { tokenRefId = txInRefId $ wuTxIn $ wtsUtxo state
                , tokenState = tokenState
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
requestJSONToRequestZoo WitnessedRequest{wrUtxo} = do
    let refId = txInRefId $ wuTxIn wrUtxo
    -- Decode the typed request from the VERIFIED inline-datum TxOut
    -- (proof-bound to the trusted root by 'verifyTokenRequests'). 0.2.0
    -- (#342) removed the unverified, lossy server-side request projection,
    -- which dropped an update's old value and so could not type
    -- accept\/reject\/finished\/config updates (they collapsed to
    -- UnknownUpdate); the inline datum carries BOTH values.
    request <- decodeRequestDatum $ wuTxOut wrUtxo
    requestDatumToJSValue refId request >>= parseRequestZoo

-- | Decode the on-chain 'OnChainRequest' from a request UTxO's inline datum.
-- @wuTxOut@ is the CBOR of the Conway 'TxOut'; its inline datum is the cage
-- 'RequestDatum', which carries BOTH the old and new values for updates.
decodeRequestDatum :: Hex -> Either String OnChainRequest
decodeRequestDatum (Hex txOutBytes) = do
    txOut <-
        first (("request TxOut decode failed: " <>) . show)
            $ decodeFull (eraProtVerLow @ConwayEra) (BL.fromStrict txOutBytes)
    case cageDatumOf txOut of
        Just (RequestDatum request) -> Right request
        Just (StateDatum _) ->
            Left "request UTxO carries a state datum, not a request datum"
        Nothing -> Left "request UTxO has no inline cage datum"

-- | Extract the cage datum from a Conway 'TxOut' inline datum.
cageDatumOf :: TxOut ConwayEra -> Maybe CageDatum
cageDatumOf txOut = case txOut ^. datumTxOutL of
    Datum binaryData ->
        let Data plutusData = binaryDataToData binaryData
         in fromBuiltinData (BuiltinData plutusData)
    _ -> Nothing

-- | Render an 'OnChainRequest' as the @change@-bearing JSValue that
-- 'parseRequestZoo' consumes — now with the REAL old and new values.
requestDatumToJSValue
    :: RequestRefId -> OnChainRequest -> Either String JSValue
requestDatumToJSValue refId request = do
    keyValue <- canonicalBytesToJSValue "request.key" (Hex $ requestKey request)
    operationFields <- requestOperationFields $ requestValue request
    pure
        $ runIdentity
        $ object
            [ "outputRefId" .= refId
            , "owner"
                .= Owner (hexString $ fromBuiltin $ requestOwner request)
            ,
                ( "change"
                , object
                    $ ("key" .= jsonToString keyValue) : operationFields
                )
            ]

requestOperationFields
    :: OnChainOperation -> Either String [(String, Identity JSValue)]
requestOperationFields = \case
    OpInsert v -> do
        newValue <- requestValueJSON v
        pure
            [ "type" .= ("insert" :: String)
            , "newValue" .= jsonToString newValue
            ]
    OpDelete v -> do
        oldValue <- requestValueJSON v
        pure
            [ "type" .= ("delete" :: String)
            , "oldValue" .= jsonToString oldValue
            ]
    OpUpdate old new -> do
        oldValue <- requestValueJSON old
        newValue <- requestValueJSON new
        pure
            [ "type" .= ("update" :: String)
            , "oldValue" .= jsonToString oldValue
            , "newValue" .= jsonToString newValue
            ]

requestValueJSON :: BS.ByteString -> Either String JSValue
requestValueJSON = canonicalBytesToJSValue "request.value" . Hex

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

{- | Recover the token's owner and trie root from the VERIFIED state
UTxO. 0.2.0 (#342) dropped the server-side @TokenStateJSON@ projection, so
the owner\/root now come from the inline 'StateDatum' inside the proof-bound
@wuTxOut@ — the same source of truth the request path already decodes.
-}
witnessedTokenStateToTokenState
    :: WitnessedTokenState -> Either String TokenState
witnessedTokenStateToTokenState WitnessedTokenState{wtsUtxo} = do
    OnChainTokenState{stateOwner, stateRoot} <-
        decodeStateDatum $ wuTxOut wtsUtxo
    pure
        TokenState
            { tokenRoot = Root $ hexString $ unOnChainRoot stateRoot
            , tokenOwner = Owner $ hexString $ fromBuiltin stateOwner
            }

-- | Decode the on-chain 'OnChainTokenState' from a state UTxO's inline
-- datum. @wuTxOut@ is the CBOR of the Conway 'TxOut'; its inline datum is
-- the cage 'StateDatum'.
decodeStateDatum :: Hex -> Either String OnChainTokenState
decodeStateDatum (Hex txOutBytes) = do
    txOut <-
        first (("state TxOut decode failed: " <>) . show)
            $ decodeFull (eraProtVerLow @ConwayEra) (BL.fromStrict txOutBytes)
    case cageDatumOf txOut of
        Just (StateDatum state) -> Right state
        Just (RequestDatum _) ->
            Left "state UTxO carries a request datum, not a state datum"
        Nothing -> Left "state UTxO has no inline cage datum"

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
