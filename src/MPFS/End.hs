module MPFS.End
    ( endTokenFromFacts
    , tokenIdJSONFromTokenId
    ) where

import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( EndFacts
    , EndRequest (..)
    , StatusResponse (..)
    )
import Cardano.MPFS.API.Types.Common (TokenIdJSON (..))
import Cardano.MPFS.Client.Cage.End (endCageTxWithEval)
import Cardano.MPFS.Client.Facts (verifyEndFacts)
import Cardano.MPFS.Client.TrustedRoot (TrustedRoot (..))
import Control.Monad.IO.Class (liftIO)
import Core.Types.Basic
    ( Address
    , TokenId (..)
    )
import Core.Types.Tx
    ( UnsignedTx (..)
    , WithUnsignedTx (..)
    )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as B
import MPFS.Cage
    ( addressBytesForCage
    , awaitStatus
    , liftEitherClientM
    , loadCageConfig
    , resolveEvalContext
    , txHex
    , walletPolicy
    )
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue)

endTokenFromFacts
    :: ClientM StatusResponse
    -> (EndRequest -> ClientM EndFacts)
    -> Address
    -> TokenId
    -> ClientM (WithUnsignedTx JSValue)
endTokenFromFacts getStatus postEndFacts address tokenId = do
    rawAddress <- liftEitherClientM $ addressBytesForCage address
    token <- liftEitherClientM $ tokenIdJSONFromTokenId tokenId
    StatusResponse{currentUtxoRoot} <- awaitStatus getStatus
    trustedRoot <-
        liftEitherClientM $ maybeToEither noUtxoRoot currentUtxoRoot
    facts <- postEndFacts $ EndRequest token $ Hex rawAddress
    cfg <- liftIO $ loadCageConfig rawAddress
    verified <-
        liftEitherClientM
            $ firstShow
            $ verifyEndFacts cfg (TrustedRoot trustedRoot) facts
    evalCtx <- resolveEvalContext
    tx <-
        liftEitherClientM
            $ firstShow
            $ endCageTxWithEval evalCtx cfg walletPolicy verified
    pure
        $ WithUnsignedTx
            (UnsignedTx $ txHex tx)
            Nothing
  where
    noUtxoRoot =
        "GET /status returned no utxo_root; MPFS indexer is not ready"
    firstShow :: Show e => Either e a -> Either String a
    firstShow =
        either (Left . show) Right

tokenIdJSONFromTokenId :: TokenId -> Either String TokenIdJSON
tokenIdJSONFromTokenId (TokenId tokenId) =
    TokenIdJSON <$> Base16.decode (B.pack tokenId)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e =
    maybe (Left e) Right
