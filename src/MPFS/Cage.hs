{-# LANGUAGE NumericUnderscores #-}

module MPFS.Cage
    ( addressBytesForCage
    , liftEitherClientM
    , loadCageConfig
    , resolveEvalContext
    , txHex
    , walletPolicy
    ) where

import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Binary (natVersion, serialize')
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Plutus.ExUnits (Prices (..))
import Cardano.MPFS.API (EvalContextAPI)
import Cardano.MPFS.API.Types (EvalContext)
import Cardano.MPFS.Cage.Blueprint
    ( Blueprint
    , extractCompiledCode
    , loadBlueprint
    )
import Cardano.MPFS.Client.Cage.Config
    ( CageConfig (..)
    , computeScriptHash
    )
import Cardano.MPFS.Client
    ( DecodedEvalContext
    , decodeEvalContext
    )
import Cardano.MPFS.Client.Cage.Policy (WalletPolicy (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Codec.Binary.Bech32 qualified as Bech32
import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Core.Types.Basic (Address (..))
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as SBS
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Servant.Client (ClientM, client)
import System.Environment (lookupEnv)

import Cardano.Tx.Ledger (ConwayTx)

addressBytesForCage :: Address -> Either String ByteString
addressBytesForCage (Address address) = do
    (_, dataPart) <-
        firstShow
            $ Bech32.decodeLenient address
    maybe
        (Left "Address Bech32 data part is not byte-aligned")
        Right
        (Bech32.dataPartToBytes dataPart)

liftEitherClientM :: Either String a -> ClientM a
liftEitherClientM =
    either (liftIO . throwIO . userError) pure

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

resolveEvalContext :: ClientM DecodedEvalContext
resolveEvalContext = do
    wire <- evalContextClient
    liftEitherClientM $ firstShow $ decodeEvalContext wire
  where
    evalContextClient :: ClientM EvalContext
    evalContextClient =
        client (Proxy @EvalContextAPI)

walletPolicy :: WalletPolicy
walletPolicy =
    WalletPolicy
        { wpMaxFee = Coin 10_000_000
        , wpMaxExUnitPrices = Prices maxBound maxBound
        , wpMaxMinUtxoCoinPerByte = Coin 10_000
        , wpMaxValidityWindow = SlotNo maxBound
        }

txHex :: ConwayTx -> T.Text
txHex =
    T.decodeUtf8 . Base16.encode . serialize' (natVersion @11)

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

firstShow :: Show e => Either e a -> Either String a
firstShow =
    either (Left . show) Right
