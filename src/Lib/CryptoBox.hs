module Lib.CryptoBox
    ( Nonce192 (..)
    , encryptOnly
    , encryptAndSign
    , decryptOnly
    , verifiyAndDecrypt
    , mkNonce
    )
where

import Crypto.Box (create, open)
import Crypto.PubKey.Ed25519
    ( PublicKey
    , SecretKey
    , generateSecretKey
    , toPublic
    )
import Crypto.Random
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Word (Word8)
import Lib.EdToCurve (convertPublicKey, convertSecretKey)

newtype Nonce192 = Nonce192 ByteString
    deriving (Show, Eq)

mkNonce :: ByteString -> Maybe Nonce192
mkNonce bs
    | B.length bs >= 24 = Just (Nonce192 $ B.take 24 bs)
    | otherwise = Nothing

encryptAndSign
    :: PublicKey
    -- ^ Recipient's public key
    -> SecretKey
    -- ^ Sender's secret key
    -> ByteString
    -- ^ Plaintext message
    -> Nonce192
    -- ^ nonce
    -> Either String ByteString
encryptAndSign pk sk msg (Nonce192 nonce) = do
    pk' <- convertPublicKey pk
    sk' <- convertSecretKey sk
    Right $ create msg nonce pk' sk'

verifiyAndDecrypt
    :: PublicKey
    -- ^ Sender's public key
    -> SecretKey
    -- ^ Recipient's secret key
    -> ByteString
    -- ^ Ciphertext message
    -> Nonce192
    -- ^ nonce
    -> Either String (Maybe ByteString)
verifiyAndDecrypt pk sk msg (Nonce192 nonce) = do
    pk' <- convertPublicKey pk
    sk' <- convertSecretKey sk
    Right $ open msg nonce pk' sk'

{-
  i.e.
 As the agent already has to prove to the oracle his identity and the oracle
 is in charge of deciding/distributing his identity, there is no need for the agent
 to authenticate his messages to the requester as the oracle can already forge them
 and noone else can impersonate the agent without being detected by the oracle.

 This is one case where we can use this key pair as it is not a security issue
 if someone else knows it.
 -}

newtype ConstantSeed a = ConstantSeed ([Word8] -> a)
    deriving (Functor, Applicative, Monad)

runConstantSeed :: ConstantSeed a -> [Word8] -> a
runConstantSeed (ConstantSeed f) = f

instance MonadRandom ConstantSeed where
    getRandomBytes n = ConstantSeed $ \bytes ->
        let (h, _) = splitAt n bytes
        in  BA.pack h

disclosedSKey :: SecretKey
disclosedSKey = runConstantSeed generateSecretKey [0 ..]

disclosedPKey :: PublicKey
disclosedPKey = toPublic disclosedSKey

encryptOnly
    :: PublicKey -> ByteString -> Nonce192 -> Either String ByteString
encryptOnly pk = encryptAndSign pk disclosedSKey

decryptOnly
    :: SecretKey
    -> ByteString
    -> Nonce192
    -> Either String (Maybe ByteString)
decryptOnly = verifiyAndDecrypt disclosedPKey
