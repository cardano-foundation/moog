module MPFS.Update
    ( updateTokenFromFacts
    , updateTokenFactsRequest
    ) where

import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( StatusResponse (..)
    , UpdateRequest (..)
    )
import Cardano.MPFS.API.Types.Facts (UpdateFacts)
import Cardano.MPFS.Client.Cage.Update (updateCageTxWithEval)
import Cardano.MPFS.Client.Facts (verifyUpdateFacts)
import Cardano.MPFS.Client.TrustedRoot (TrustedRoot (..))
import Control.Monad.IO.Class (liftIO)
import Core.Types.Basic
    ( Address
    , RequestRefId (requestId)
    , TokenId
    )
import Core.Types.Tx
    ( UnsignedTx (..)
    , WithUnsignedTx (..)
    )
import MPFS.Cage
    ( addressBytesForCage
    , awaitStatus
    , liftEitherClientM
    , loadCageConfig
    , resolveEvalContext
    , txHex
    , walletPolicy
    )
import MPFS.End (tokenIdJSONFromTokenId)
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue)

updateTokenFromFacts
    :: ClientM StatusResponse
    -> (UpdateRequest -> ClientM UpdateFacts)
    -> Address
    -> TokenId
    -> [RequestRefId]
    -> ClientM (WithUnsignedTx JSValue)
updateTokenFromFacts getStatus postFacts address tokenId requests = do
    request <- liftEitherClientM $ updateTokenFactsRequest address tokenId requests
    StatusResponse{currentUtxoRoot} <- awaitStatus getStatus
    trustedRoot <-
        liftEitherClientM $ maybeToEither noUtxoRoot currentUtxoRoot
    facts <- postFacts request
    verified <-
        liftEitherClientM
            $ firstShow
            $ verifyUpdateFacts (TrustedRoot trustedRoot) facts
    rawAddress <- liftEitherClientM $ addressBytesForCage address
    cfg <- liftIO $ loadCageConfig rawAddress
    evalCtx <- resolveEvalContext
    tx <-
        liftEitherClientM
            $ firstShow
            $ updateCageTxWithEval evalCtx cfg walletPolicy verified
    pure $ WithUnsignedTx (UnsignedTx $ txHex tx) Nothing

updateTokenFactsRequest
    :: Address -> TokenId -> [RequestRefId] -> Either String UpdateRequest
updateTokenFactsRequest address tokenId requests =
    UpdateRequest
        <$> tokenIdJSONFromTokenId tokenId
        <*> (Hex <$> addressBytesForCage address)
        <*> pure (requestId <$> requests)

firstShow :: Show e => Either e a -> Either String a
firstShow =
    either (Left . show) Right

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e =
    maybe (Left e) Right

noUtxoRoot :: String
noUtxoRoot =
    "GET /status returned no utxo_root; MPFS indexer is not ready"
