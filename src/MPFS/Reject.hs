module MPFS.Reject
    ( rejectTokenFromFacts
    , rejectTokenFactsRequest
    ) where

import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( RejectRequest (..)
    , StatusResponse (..)
    )
import Cardano.MPFS.API.Types.Facts (RejectFacts)
import Cardano.MPFS.Client.Cage.Reject (rejectCageTxWithEval)
import Cardano.MPFS.Client.Facts (verifyRejectFacts)
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
    , liftEitherClientM
    , loadCageConfig
    , resolveEvalContext
    , txHex
    , walletPolicy
    )
import MPFS.End (tokenIdJSONFromTokenId)
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue)

rejectTokenFromFacts
    :: ClientM StatusResponse
    -> (RejectRequest -> ClientM RejectFacts)
    -> Address
    -> TokenId
    -> [RequestRefId]
    -> ClientM (WithUnsignedTx JSValue)
rejectTokenFromFacts getStatus postFacts address tokenId requests = do
    request <- liftEitherClientM $ rejectTokenFactsRequest address tokenId requests
    StatusResponse{currentUtxoRoot} <- getStatus
    trustedRoot <-
        liftEitherClientM $ maybeToEither noUtxoRoot currentUtxoRoot
    facts <- postFacts request
    verified <-
        liftEitherClientM
            $ firstShow
            $ verifyRejectFacts (TrustedRoot trustedRoot) facts
    rawAddress <- liftEitherClientM $ addressBytesForCage address
    cfg <- liftIO $ loadCageConfig rawAddress
    evalCtx <- resolveEvalContext
    tx <-
        liftEitherClientM
            $ firstShow
            $ rejectCageTxWithEval evalCtx cfg walletPolicy verified
    pure $ WithUnsignedTx (UnsignedTx $ txHex tx) Nothing

rejectTokenFactsRequest
    :: Address -> TokenId -> [RequestRefId] -> Either String RejectRequest
rejectTokenFactsRequest address tokenId requests =
    RejectRequest
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
