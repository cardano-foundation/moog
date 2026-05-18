module MPFS.Boot
    ( addressBytesForBoot
    , bootTokenFromFacts
    ) where

import Cardano.Ledger.Mary.Value
    ( AssetName (..)
    , MultiAsset (..)
    )
import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( BootFacts
    , BootRequest (..)
    , StatusResponse (..)
    )
import Cardano.MPFS.Cage.Ledger (ConwayEra)
import Cardano.MPFS.Client.Cage.Boot (bootCageTx)
import Cardano.MPFS.Client.Cage.Config
    ( CageConfig (..)
    , cagePolicyIdFromCfg
    )
import Cardano.MPFS.Client.Facts (verifyBootFacts)
import Cardano.MPFS.Client.TrustedRoot (TrustedRoot (..))
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Core.Types.Basic (Address (..))
import Core.Types.Tx
    ( UnsignedTx (..)
    , WithUnsignedTx (..)
    )
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue (..), toJSString)

import Cardano.Ledger.Api.Tx (Tx, bodyTxL)
import Cardano.Ledger.Api.Tx.Body (mintTxBodyL)
import MPFS.Cage
    ( addressBytesForCage
    , liftEitherClientM
    , loadCageConfig
    , txHex
    , walletPolicy
    )

addressBytesForBoot :: Address -> Either String ByteString
addressBytesForBoot =
    addressBytesForCage

bootTokenFromFacts
    :: ClientM StatusResponse
    -> (BootRequest -> ClientM BootFacts)
    -> Address
    -> ClientM (WithUnsignedTx JSValue)
bootTokenFromFacts getStatus postBootFacts address = do
    rawAddress <- liftEitherClientM $ addressBytesForBoot address
    StatusResponse{currentUtxoRoot} <- getStatus
    trustedRoot <-
        liftEitherClientM $ maybeToEither noUtxoRoot currentUtxoRoot
    facts <- postBootFacts $ BootRequest $ Hex rawAddress
    verified <-
        liftEitherClientM
            $ firstShow
            $ verifyBootFacts (TrustedRoot trustedRoot) facts
    cfg <- liftIO $ loadCageConfig rawAddress
    tx <-
        liftEitherClientM $ firstShow $ bootCageTx cfg walletPolicy verified
    tokenId <- liftEitherClientM $ extractTokenId cfg tx
    pure
        $ WithUnsignedTx
            (UnsignedTx $ txHex tx)
            (Just $ tokenIdValue tokenId)
  where
    noUtxoRoot =
        "GET /status returned no utxo_root; MPFS indexer is not ready"
    firstShow :: Show e => Either e a -> Either String a
    firstShow =
        either (Left . show) Right

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e =
    maybe (Left e) Right

extractTokenId
    :: CageConfig
    -> Tx ConwayEra
    -> Either String ByteString
extractTokenId cfg tx =
    let MultiAsset minted =
            tx ^. bodyTxL . mintTxBodyL
        pid = cagePolicyIdFromCfg cfg
    in  case Map.lookup pid minted >>= oneAsset of
            Just (AssetName tokenName) -> Right $ SBS.fromShort tokenName
            Nothing -> Left "boot transaction did not mint exactly one cage token"

oneAsset :: Map.Map AssetName Integer -> Maybe AssetName
oneAsset assets =
    case Map.toList assets of
        [(assetName, _)] -> Just assetName
        _ -> Nothing

tokenIdValue :: ByteString -> JSValue
tokenIdValue =
    JSString . toJSString . T.unpack . T.decodeUtf8 . Base16.encode
