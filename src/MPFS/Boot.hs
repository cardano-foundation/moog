{-# LANGUAGE NumericUnderscores #-}

module MPFS.Boot
    ( addressBytesForBoot
    , bootTokenFromFacts
    ) where

import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Binary (natVersion, serialize')
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Value
    ( AssetName (..)
    , MultiAsset (..)
    )
import Cardano.Ledger.Plutus.ExUnits (Prices (..))
import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( BootFacts
    , BootRequest (..)
    , StatusResponse (..)
    )
import Cardano.MPFS.Cage.Blueprint
    ( Blueprint
    , extractCompiledCode
    , loadBlueprint
    )
import Cardano.MPFS.Cage.Ledger (ConwayEra)
import Cardano.MPFS.Client.Cage.Boot (bootCageTx)
import Cardano.MPFS.Client.Cage.Config
    ( CageConfig (..)
    , cagePolicyIdFromCfg
    , computeScriptHash
    )
import Cardano.MPFS.Client.Cage.Policy (WalletPolicy (..))
import Cardano.MPFS.Client.Facts (verifyBootFacts)
import Cardano.MPFS.Client.TrustedRoot (TrustedRoot (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Codec.Binary.Bech32 qualified as Bech32
import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Core.Types.Basic (Address (..))
import Core.Types.Tx
    ( UnsignedTx (..)
    , WithUnsignedTx (..)
    )
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Servant.Client (ClientM)
import System.Environment (lookupEnv)
import Text.JSON.Canonical (JSValue (..), toJSString)

import Cardano.Ledger.Api.Tx (Tx, bodyTxL)
import Cardano.Ledger.Api.Tx.Body (mintTxBodyL)

addressBytesForBoot :: Address -> Either String ByteString
addressBytesForBoot (Address address) = do
    (_, dataPart) <-
        firstShow
            $ Bech32.decodeLenient address
    maybe
        (Left "Address Bech32 data part is not byte-aligned")
        Right
        (Bech32.dataPartToBytes dataPart)
  where
    firstShow =
        either (Left . show) Right

bootTokenFromFacts
    :: ClientM StatusResponse
    -> (BootRequest -> ClientM BootFacts)
    -> Address
    -> ClientM (WithUnsignedTx JSValue)
bootTokenFromFacts getStatus postBootFacts address = do
    rawAddress <- liftEitherIO $ addressBytesForBoot address
    StatusResponse{currentUtxoRoot} <- getStatus
    trustedRoot <- liftEitherIO $ maybeToEither noUtxoRoot currentUtxoRoot
    facts <- postBootFacts $ BootRequest $ Hex rawAddress
    verified <-
        liftEitherIO
            $ firstShow
            $ verifyBootFacts (TrustedRoot trustedRoot) facts
    cfg <- liftIO $ loadCageConfig rawAddress
    tx <- liftEitherIO $ firstShow $ bootCageTx cfg walletPolicy verified
    tokenId <- liftEitherIO $ extractTokenId cfg tx
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

liftEitherIO :: Either String a -> ClientM a
liftEitherIO =
    either (liftIO . throwIO . userError) pure

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e =
    maybe (Left e) Right

loadCageConfig :: ByteString -> IO CageConfig
loadCageConfig rawAddress = do
    path <- requireBlueprintPath
    blueprint <- loadBlueprint path >>= either (throwIO . userError) pure
    stateBytes <- requireCompiledCode blueprint statePrefixes
    requestBytes <- requireCompiledCode blueprint requestPrefixes
    pure
        CageConfig
            { cageScriptBytes = stateBytes
            , requestScriptBytes = requestBytes
            , cfgScriptHash = computeScriptHash stateBytes
            , defaultProcessTime = 5_000
            , defaultRetractTime = 5_000
            , defaultTip = Coin 1_000_000
            , network = networkFromAddressBytes rawAddress
            }

requireBlueprintPath :: IO FilePath
requireBlueprintPath = do
    moogPath <- lookupEnv "MOOG_MPFS_BLUEPRINT"
    mpfsPath <- lookupEnv "MPFS_BLUEPRINT"
    case moogPath <|> mpfsPath of
        Just path -> pure path
        Nothing ->
            throwIO
                $ userError
                    "MOOG_MPFS_BLUEPRINT or MPFS_BLUEPRINT must point to the trusted MPFS blueprint"

requireCompiledCode
    :: Blueprint
    -> [T.Text]
    -> IO SBS.ShortByteString
requireCompiledCode blueprint prefixes =
    case firstJust (`extractCompiledCode` blueprint) prefixes of
        Just bytes -> pure bytes
        Nothing ->
            throwIO
                $ userError
                $ "compiled code not found in blueprint for prefixes: "
                    <> show prefixes

statePrefixes :: [T.Text]
statePrefixes =
    [ "state.state"
    , "state."
    ]

requestPrefixes :: [T.Text]
requestPrefixes =
    [ "request.request"
    , "request."
    ]

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f =
    foldr (\x acc -> f x <|> acc) Nothing

networkFromAddressBytes :: ByteString -> Network
networkFromAddressBytes rawAddress =
    case BS.uncons rawAddress of
        Nothing -> Testnet
        Just (header, _)
            | (header .&. 0x0f) == 1 -> Mainnet
            | otherwise -> Testnet

walletPolicy :: WalletPolicy
walletPolicy =
    WalletPolicy
        { wpMaxFee = Coin 10_000_000
        , wpMaxExUnitPrices = Prices maxBound maxBound
        , wpMaxMinUtxoCoinPerByte = Coin 10_000
        , wpMaxValidityWindow = SlotNo maxBound
        }

txHex :: Tx ConwayEra -> T.Text
txHex =
    T.decodeUtf8 . Base16.encode . serialize' (natVersion @11)

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
