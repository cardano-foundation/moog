{-# LANGUAGE ViewPatterns #-}

module Lib.SSH.Public
    ( decodeSSHPublicKey
    , Verify
    , encodePublicKey
    , encodeSSHPublicKey
    , SSHPublicKey
    , unmakeSSHPublicKey
    , makeSSHPublicKey
    , unsafeSSHPublicKey
    , renderSSHPublicKey
    ) where

import Control.Monad (when)
import Crypto.Error
    ( CryptoFailable (..)
    )
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Binary.Get (Get, getByteString, getWord32be, runGet)
import Data.Binary.Put (Put, putByteString, putWord32be, runPut)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Text.JSON.Canonical (ToJSON (..))

parseEd25519Key :: Get BC.ByteString
parseEd25519Key = do
    keyTypeLen <- getWord32be
    keyType <- getByteString (fromIntegral keyTypeLen)
    when (keyType /= "ssh-ed25519") $ fail "Expected ssh-ed25519 key type"
    keyLen <- getWord32be
    when (keyLen /= 32) $ fail "Expected 32-byte Ed25519 public key"
    getByteString 32

renderEd25519Key :: B.ByteString -> Put
renderEd25519Key key = do
    putWord32be 11 -- Length of "ssh-ed25519"
    putByteString "ssh-ed25519"
    putWord32be (fromIntegral $ B.length key)
    putByteString key

type Verify = Ed25519.Signature -> B.ByteString -> Bool

decodeSSHPublicKey :: SSHPublicKey -> Maybe Ed25519.PublicKey
decodeSSHPublicKey (unmakeSSHPublicKey -> pk) =
    let
        base64Decoded = BL.fromStrict $ Base64.decodeLenient $ BC.pack pk
        keyBytes = runGet parseEd25519Key base64Decoded
    in
        case Ed25519.publicKey keyBytes of
            CryptoPassed pubKey -> Just pubKey
            CryptoFailed _ -> Nothing

encodePublicKey
    :: Ed25519.PublicKey -> String
encodePublicKey key =
    let
        encoded = BL.toStrict $ runPut $ renderEd25519Key $ BA.convert key
    in
        BC.unpack $ Base64.encode encoded

newtype SSHPublicKey = SSHPublicKey String
    deriving (Show, Eq)

instance Monad m => ToJSON m SSHPublicKey where
    toJSON (SSHPublicKey s) = toJSON s

makeSSHPublicKey :: String -> SSHPublicKey
makeSSHPublicKey = SSHPublicKey . ("ssh-ed25519 " ++)

unsafeSSHPublicKey :: String -> SSHPublicKey
unsafeSSHPublicKey = SSHPublicKey

encodeSSHPublicKey
    :: Ed25519.PublicKey -> SSHPublicKey
encodeSSHPublicKey key = makeSSHPublicKey $ encodePublicKey key

unmakeSSHPublicKey :: SSHPublicKey -> String
unmakeSSHPublicKey (SSHPublicKey s) =
    case stripPrefix "ssh-ed25519 " s of
        Just rest -> rest
        Nothing -> error "SSHPublicKey does not start with 'ssh-ed25519 '"

renderSSHPublicKey :: SSHPublicKey -> String
renderSSHPublicKey (SSHPublicKey s) = s

stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix _ [] = Nothing
stripPrefix (x : xs) (y : ys)
    | x == y = stripPrefix xs ys
    | otherwise = Nothing
