module Lib.EdToCurve
    ( convertSecretKey
    , convertPublicKey
    ) where

import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (SHA512 (..), hashWith)
import Crypto.Number.ModArithmetic (expFast)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Crypto.PubKey.Curve25519 qualified as Curve25519
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Bits (Bits (shiftL, (.&.), (.|.)))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B

-- | Inverse modular p
modpInv :: Integer -> Integer
modpInv x = expFast x (p - 2) p

-- Prime for Curve25519/Ed25519 field
p :: Integer
p = (1 `shiftL` 255) - 19

mask255 :: Integer
mask255 = (1 `shiftL` 255) - 1

-- Convert Ed25519 public key (32 bytes) -> X25519 public key (32 bytes)
pubToX25519 :: ByteString -> Either String ByteString
pubToX25519 edpub =
    let full = os2ip . B.reverse $ edpub
        y = full .&. mask255
        num = (1 + y) `mod` p
        den = (1 - y) `mod` p
        u = (num * modpInv den) `mod` p
    in  if den == 0
            then Left "edPubToX25519: invalid point (1 - y == 0)"
            else Right $ B.reverse . i2ospOf_ 32 $ u

-- Convert Ed25519 32-byte seed -> X25519 scalar (32 bytes)
seedToScalar :: ByteString -> Either String ByteString
seedToScalar seed =
    let
        d = convert @_ @ByteString $ hashWith SHA512 seed
        s = B.take 32 d
        b0 = B.singleton $ B.index s 0 .&. 248
        b31 = B.singleton $ (B.index s 31 .&. 127) .|. 64
        rest = B.take 30 $ B.drop 1 s
    in
        Right $ B.concat [b0, rest, b31]

convertSecretKey
    :: Ed25519.SecretKey
    -> Either String Curve25519.SecretKey
convertSecretKey sk = do
    scalar <- seedToScalar $ convert sk
    case Curve25519.secretKey scalar of
        CryptoFailed err -> Left $ show err
        CryptoPassed sk' -> Right sk'

convertPublicKey
    :: Ed25519.PublicKey
    -> Either String Curve25519.PublicKey
convertPublicKey pk = do
    u <- pubToX25519 $ convert pk
    case Curve25519.publicKey u of
        CryptoFailed err -> Left $ show err
        CryptoPassed pk' -> Right pk'
