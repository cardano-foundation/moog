{-# LANGUAGE TypeFamilies #-}

module Core.Encryption
    ( encrypt
    , decrypt
    , encryptText
    , decryptText
    ) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
    ( BlockCipher (..)
    , Cipher (..)
    , IV
    , KeySizeSpecifier (..)
    , makeIV
    )
import Crypto.Error (CryptoError (..), CryptoFailable (..))
import Crypto.KDF.BCryptPBKDF (Parameters (..), generate)
import Crypto.Random.Types qualified as CRT
import Data.ByteArray (convert)
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text.Encoding
import Data.Word (Word8)

-- | Type level state to represent whether the message is encrypted or decrypted.
data State = EncryptedS | DecryptedS
    deriving (Show, Eq)

-- | Type family to switch between encrypted and decrypted states.
type family Sym (s :: State) where
    Sym 'EncryptedS = 'DecryptedS
    Sym 'DecryptedS = 'EncryptedS

-- | Message type that can be either encrypted or decrypted.
data Message s where
    Encrypted :: ByteString -> Message 'EncryptedS
    Decrypted :: ByteString -> Message 'DecryptedS

-- | Salt used for key derivation.
newtype Salt = Salt ByteString
    deriving (Show, Eq)

-- | A data type to hold the encrypted/decrypted message along with its IV and salt.
data Crypted s = Crypted
    { message :: Message s
    , iv :: IV AES256
    , salt :: Salt
    , kdfRuns :: Word8
    }

keySize :: Int
keySize = case cipherKeySize (undefined :: AES256) of
    KeySizeFixed n -> n
    _ -> error "Unsupported key size for AES256"

generateSalt :: IO Salt
generateSalt = Salt <$> CRT.getRandomBytes keySize

generateKey :: Word8 -> ByteString -> ByteString -> ByteString
generateKey iterCounts =
    generate
        ( Parameters
            { iterCounts = fromIntegral iterCounts
            , outputLength = keySize
            }
        )

genRandomIV :: IO (IV AES256)
genRandomIV = do
    bytes :: ByteString <-
        CRT.getRandomBytes $ blockSize (undefined :: AES256)
    case makeIV bytes of
        Nothing -> error "Failed to generate an initialization vector."
        Just iv -> pure iv

initCipher :: ByteString -> Either CryptoError AES256
initCipher k = case cipherInit k of
    CryptoFailed e -> Left e
    CryptoPassed a -> Right a

renderEncrypted :: Crypted EncryptedS -> ByteString
renderEncrypted (Crypted (Encrypted msg) iv (Salt salt) runs) =
    B.cons runs
        $ salt <> convert iv <> msg

parseEncrypted :: ByteString -> Maybe (Crypted EncryptedS)
parseEncrypted bs = do
    (kdfRuns, runsRest) <- B.uncons bs
    let (salt, saltRest) = B.splitAt keySize runsRest
        (ivBytes, message) = B.splitAt (blockSize (undefined :: AES256)) saltRest
    iv <- makeIV ivBytes
    pure
        $ Crypted
            { message = Encrypted message
            , iv
            , salt = Salt salt
            , kdfRuns = kdfRuns
            }

sym
    :: ByteString
    -> Crypted s
    -> Either CryptoError (Crypted (Sym s))
sym passphrase crypted@Crypted{message, iv, salt = Salt salt, kdfRuns} =
    case message of
        Encrypted m ->
            symcore m <&> \decryptedMsg -> crypted{message = Decrypted decryptedMsg}
        Decrypted m ->
            symcore m <&> \encryptedMsg -> crypted{message = Encrypted encryptedMsg}
  where
    key = generateKey kdfRuns passphrase salt
    symcore :: ByteString -> Either CryptoError ByteString
    symcore m = case initCipher key of
        Left e ->
            error $ show (e, B.length key, cipherKeySize (undefined :: AES256))
        Right c -> Right $ ctrCombine c iv m

-- | One time encryption
encrypt
    :: ByteString
    -- ^ passphrase
    -> Word8
    -- ^ number of key derivation function runs
    -> ByteString
    -- ^ message to encrypt
    -> IO ByteString
    -- ^ encrypted message
encrypt passphrase kdfRuns message = do
    salt <- generateSalt
    iv <- genRandomIV
    case sym passphrase Crypted{message = Decrypted message, iv, salt, kdfRuns} of
        Left e -> error $ show e
        Right encrypted ->
            pure $ renderEncrypted encrypted

-- | Any time decryption
decrypt
    :: ByteString
    -- ^ passphrase
    -> ByteString
    -- ^ encrypted message
    -> Either String ByteString
decrypt passphrase encrypted = do
    case parseEncrypted encrypted of
        Nothing -> error "Failed to parse encrypted message iv"
        Just crypted -> do
            case sym passphrase crypted of
                Left e -> error $ "Decryption failed: " ++ show e
                Right Crypted{message = Decrypted decrypted} -> pure decrypted

-- | Encrypts a Text message using the provided passphrase and KDF runs.
encryptText
    :: Text
    -- ^ passphrase
    -> Word8
    -- ^ number of key derivation function runs
    -> Text
    -- ^ message to encrypt
    -> IO Text
    -- ^ encrypted message
encryptText passphrase kdfRuns message = do
    encrypted <-
        encrypt (encodeUtf8 passphrase) kdfRuns (encodeUtf8 message)
    pure $ decodeUtf8 $ convertToBase Base16 encrypted

-- | Decrypts a Text message using the provided passphrase.
decryptText
    :: Text
    -- ^ passphrase
    -> Text
    -- ^ encrypted message
    -> Either String Text
    -- ^ decrypted message
decryptText passphrase encrypted = do
    let encryptedBS = convertFromBase Base16 (encodeUtf8 encrypted)
    case encryptedBS of
        Left _err -> Left "Decryption failed: invalid base16 message"
        Right encBS -> do
            decrypted <- decrypt (encodeUtf8 passphrase) encBS
            case decodeUtf8' decrypted of
                Left _err -> Left "Decryption failed: invalid UTF8 decrypted"
                Right txt -> pure txt
