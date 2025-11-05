module Core.Types.VKey
    ( VKey (..)
    , encodeVKey
    , decodeVKey
    , EncodeVKeyError (..)
    , DecodeVKeyError (..)
    ) where

import Codec.Binary.Bech32
    ( DecodingError
    , EncodingError
    , HumanReadablePartError
    , dataPartFromBytes
    , dataPartToBytes
    , decode
    , encode
    , humanReadablePartFromText
    , humanReadablePartToText
    )
import Crypto.Error (CryptoError, CryptoFailable (..))
import Crypto.PubKey.Ed25519 (PublicKey, publicKey)
import Data.ByteArray (convert)
import Data.Text (Text)

newtype VKey = VKey Text deriving (Show, Eq)

data EncodeVKeyError
    = Bech32HrpError HumanReadablePartError
    | Bech32EncodeError EncodingError
    deriving (Eq, Show)

encodeVKey :: PublicKey -> Either EncodeVKeyError VKey
encodeVKey pk =
    case humanReadablePartFromText "vkey" of
        Left err ->
            Left $ Bech32HrpError err
        Right hrp ->
            let data32 = dataPartFromBytes $ convert pk
            in  case encode hrp data32 of
                    Left err -> Left $ Bech32EncodeError err
                    Right bech32PubKey -> Right $ VKey bech32PubKey

data DecodeVKeyError
    = Bech32DecodeError DecodingError
    | Bech32HrpMismatch Text
    | Bech32DataPartError
    | Ed25519PublicKeyError CryptoError
    deriving (Eq, Show)

decodeVKey :: VKey -> Either DecodeVKeyError PublicKey
decodeVKey (VKey text) =
    case decode text of
        Left err -> Left $ Bech32DecodeError err
        Right (hrp, dp) ->
            if humanReadablePartToText hrp /= "vkey"
                then Left $ Bech32HrpMismatch $ humanReadablePartToText hrp
                else case dataPartToBytes dp of
                    Nothing -> Left Bech32DataPartError
                    Just bytes ->
                        case publicKey bytes of
                            CryptoFailed err ->
                                Left $ Ed25519PublicKeyError err
                            CryptoPassed pk -> Right pk
