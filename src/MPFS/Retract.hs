module MPFS.Retract
    ( retractChangeFromFacts
    , retractFactsRequest
    ) where

import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( RetractRequest (..)
    , StatusResponse (..)
    )
import Cardano.MPFS.Client.Cage.Retract (retractCageTx)
import Cardano.MPFS.Client.Facts
    ( RetractFacts
    , verifyRetractFacts
    )
import Cardano.MPFS.Client.TrustedRoot (TrustedRoot (..))
import Control.Monad.IO.Class (liftIO)
import Core.Types.Basic
    ( Address
    , RequestRefId (..)
    )
import Core.Types.Tx
    ( UnsignedTx (..)
    , WithUnsignedTx (..)
    )
import MPFS.Cage
    ( addressBytesForCage
    , liftEitherClientM
    , loadCageConfig
    , txHex
    , walletPolicy
    )
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue)

retractChangeFromFacts
    :: ClientM StatusResponse
    -> (RetractRequest -> ClientM RetractFacts)
    -> Address
    -> RequestRefId
    -> ClientM (WithUnsignedTx JSValue)
retractChangeFromFacts getStatus postFacts address requestRef = do
    request <- liftEitherClientM $ retractFactsRequest address requestRef
    StatusResponse{currentUtxoRoot} <- getStatus
    trustedRoot <-
        liftEitherClientM $ maybeToEither noUtxoRoot currentUtxoRoot
    facts <- postFacts request
    verified <-
        liftEitherClientM
            $ firstShow
            $ verifyRetractFacts (TrustedRoot trustedRoot) facts
    rawAddress <- liftEitherClientM $ addressBytesForCage address
    cfg <- liftIO $ loadCageConfig rawAddress
    tx <-
        liftEitherClientM $ firstShow $ retractCageTx cfg walletPolicy verified
    pure $ WithUnsignedTx (UnsignedTx $ txHex tx) Nothing

retractFactsRequest
    :: Address -> RequestRefId -> Either String RetractRequest
retractFactsRequest address requestRef =
    RetractRequest
        (requestId requestRef)
        <$> (Hex <$> addressBytesForCage address)

firstShow :: Show e => Either e a -> Either String a
firstShow =
    either (Left . show) Right

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e =
    maybe (Left e) Right

noUtxoRoot :: String
noUtxoRoot =
    "GET /status returned no utxo_root; MPFS indexer is not ready"
