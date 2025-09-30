module Lib.SSH.Public
    ( decodePublicKey
    , Verify
    , encodePublicKey
    , encodeSSHPublicKey
    , SSHPublicKey (..)
    , toPublicKeyHash
    , fromPublicKeyHash
    ) where

import Control.Monad (when)
import Core.Types.Basic (PublicKeyHash (..))
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

decodePublicKey
    :: PublicKeyHash -> Maybe (Verify, Ed25519.PublicKey)
decodePublicKey (PublicKeyHash pk) =
    let
        base64Decoded = BL.fromStrict $ Base64.decodeLenient $ BC.pack pk
        keyBytes = runGet parseEd25519Key base64Decoded
    in
        case Ed25519.publicKey keyBytes of
            CryptoPassed pubKey ->
                Just
                    ( flip (Ed25519.verify pubKey)
                    , pubKey
                    )
            CryptoFailed _ -> Nothing

encodePublicKey
    :: Ed25519.PublicKey -> PublicKeyHash
encodePublicKey key =
    let
        encoded = BL.toStrict $ runPut $ renderEd25519Key $ BA.convert key
    in
        PublicKeyHash $ BC.unpack $ Base64.encode encoded

newtype SSHPublicKey = SSHPublicKey String
    deriving (Show, Eq)

encodeSSHPublicKey
    :: Ed25519.PublicKey -> SSHPublicKey
encodeSSHPublicKey key =
    let
        PublicKeyHash encoded = encodePublicKey key
    in
        SSHPublicKey $ "ssh-ed25519 " <> encoded

toPublicKeyHash :: SSHPublicKey -> PublicKeyHash
toPublicKeyHash (SSHPublicKey sshPk) =
    let
        expectedPrefix = "ssh-ed25519 " :: String
    in
        PublicKeyHash $ drop (length expectedPrefix) sshPk

fromPublicKeyHash :: PublicKeyHash -> SSHPublicKey
fromPublicKeyHash (PublicKeyHash pk) =
    SSHPublicKey $ "ssh-ed25519 " <> pk
