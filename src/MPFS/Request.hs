module MPFS.Request
    ( RequestInsertBody (..)
    , RequestDeleteBody (..)
    , RequestUpdateBody (..)
    , requestInsertFromFacts
    , requestDeleteFromFacts
    , requestUpdateFromFacts
    , requestInsertFactsRequest
    , requestDeleteFactsRequest
    , requestUpdateFactsRequest
    ) where

import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( DeleteRequest (..)
    , InsertRequest (..)
    , RequestDeleteFacts
    , RequestInsertFacts
    , RequestUpdateFacts
    , StatusResponse (..)
    , UpdateValueRequest (..)
    )
import Cardano.MPFS.API.Types.Common (TokenIdJSON)
import Cardano.MPFS.Client.Cage.Request
    ( requestDeleteCageTx
    , requestInsertCageTx
    , requestUpdateCageTx
    )
import Cardano.MPFS.Client.Facts
    ( verifyRequestDeleteFacts
    , verifyRequestInsertFacts
    , verifyRequestUpdateFacts
    )
import Cardano.MPFS.Client.TrustedRoot (TrustedRoot (..))
import Control.Monad.IO.Class (liftIO)
import Core.Types.Basic
    ( Address
    , TokenId
    )
import Core.Types.Tx
    ( UnsignedTx (..)
    , WithUnsignedTx (..)
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.ByteString.Lazy qualified as BL
import Lib.JSON.Canonical.Extra
    ( fromAesonString
    , toAesonString
    )
import MPFS.Cage
    ( addressBytesForCage
    , awaitStatus
    , liftEitherClientM
    , loadCageConfig
    , txHex
    , walletPolicy
    )
import MPFS.End (tokenIdJSONFromTokenId)
import Servant.Client (ClientM)
import Text.JSON.Canonical
    ( JSValue
    , renderCanonicalJSON
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

requestInsertFromFacts
    :: ClientM StatusResponse
    -> (InsertRequest -> ClientM RequestInsertFacts)
    -> Address
    -> TokenId
    -> RequestInsertBody
    -> ClientM (WithUnsignedTx JSValue)
requestInsertFromFacts getStatus postFacts address tokenId body = do
    request <-
        liftEitherClientM
            $ requestInsertFactsRequest address tokenId body
    StatusResponse{currentUtxoRoot} <- awaitStatus getStatus
    trustedRoot <-
        liftEitherClientM $ maybeToEither noUtxoRoot currentUtxoRoot
    facts <- postFacts request
    verified <-
        liftEitherClientM
            $ firstShow
            $ verifyRequestInsertFacts (TrustedRoot trustedRoot) facts
    rawAddress <- liftEitherClientM $ addressBytesForCage address
    cfg <- liftIO $ loadCageConfig rawAddress
    tx <-
        liftEitherClientM
            $ firstShow
            $ requestInsertCageTx cfg walletPolicy verified
    pure $ WithUnsignedTx (UnsignedTx $ txHex tx) Nothing

requestDeleteFromFacts
    :: ClientM StatusResponse
    -> (DeleteRequest -> ClientM RequestDeleteFacts)
    -> Address
    -> TokenId
    -> RequestDeleteBody
    -> ClientM (WithUnsignedTx JSValue)
requestDeleteFromFacts getStatus postFacts address tokenId body = do
    request <-
        liftEitherClientM
            $ requestDeleteFactsRequest address tokenId body
    StatusResponse{currentUtxoRoot} <- awaitStatus getStatus
    trustedRoot <-
        liftEitherClientM $ maybeToEither noUtxoRoot currentUtxoRoot
    facts <- postFacts request
    verified <-
        liftEitherClientM
            $ firstShow
            $ verifyRequestDeleteFacts (TrustedRoot trustedRoot) facts
    rawAddress <- liftEitherClientM $ addressBytesForCage address
    cfg <- liftIO $ loadCageConfig rawAddress
    tx <-
        liftEitherClientM
            $ firstShow
            $ requestDeleteCageTx cfg walletPolicy verified
    pure $ WithUnsignedTx (UnsignedTx $ txHex tx) Nothing

requestUpdateFromFacts
    :: ClientM StatusResponse
    -> (UpdateValueRequest -> ClientM RequestUpdateFacts)
    -> Address
    -> TokenId
    -> RequestUpdateBody
    -> ClientM (WithUnsignedTx JSValue)
requestUpdateFromFacts getStatus postFacts address tokenId body = do
    request <-
        liftEitherClientM
            $ requestUpdateFactsRequest address tokenId body
    StatusResponse{currentUtxoRoot} <- awaitStatus getStatus
    trustedRoot <-
        liftEitherClientM $ maybeToEither noUtxoRoot currentUtxoRoot
    facts <- postFacts request
    verified <-
        liftEitherClientM
            $ firstShow
            $ verifyRequestUpdateFacts (TrustedRoot trustedRoot) facts
    rawAddress <- liftEitherClientM $ addressBytesForCage address
    cfg <- liftIO $ loadCageConfig rawAddress
    tx <-
        liftEitherClientM
            $ firstShow
            $ requestUpdateCageTx cfg walletPolicy verified
    pure $ WithUnsignedTx (UnsignedTx $ txHex tx) Nothing

requestInsertFactsRequest
    :: Address -> TokenId -> RequestInsertBody -> Either String InsertRequest
requestInsertFactsRequest address tokenId RequestInsertBody{..} =
    InsertRequest
        <$> tokenIdJSONFromTokenId' tokenId
        <*> pure (canonicalHex key)
        <*> pure (canonicalHex value)
        <*> addressHex address

requestDeleteFactsRequest
    :: Address -> TokenId -> RequestDeleteBody -> Either String DeleteRequest
requestDeleteFactsRequest address tokenId RequestDeleteBody{..} =
    DeleteRequest
        <$> tokenIdJSONFromTokenId' tokenId
        <*> pure (canonicalHex key)
        <*> pure (canonicalHex value)
        <*> addressHex address

requestUpdateFactsRequest
    :: Address -> TokenId -> RequestUpdateBody -> Either String UpdateValueRequest
requestUpdateFactsRequest address tokenId RequestUpdateBody{..} =
    UpdateValueRequest
        <$> tokenIdJSONFromTokenId' tokenId
        <*> pure (canonicalHex key)
        <*> pure (canonicalHex oldValue)
        <*> pure (canonicalHex newValue)
        <*> addressHex address

addressHex :: Address -> Either String Hex
addressHex =
    fmap Hex . addressBytesForCage

canonicalHex :: JSValue -> Hex
canonicalHex =
    Hex . BL.toStrict . renderCanonicalJSON

tokenIdJSONFromTokenId' :: TokenId -> Either String TokenIdJSON
tokenIdJSONFromTokenId' tokenId =
    case tokenIdJSONFromTokenId tokenId of
        Right token -> Right token
        Left err -> Left $ "invalid token id hex: " <> err

firstShow :: Show e => Either e a -> Either String a
firstShow =
    either (Left . show) Right

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e =
    maybe (Left e) Right

noUtxoRoot :: String
noUtxoRoot =
    "GET /status returned no utxo_root; MPFS indexer is not ready"
