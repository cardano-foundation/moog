{-
This module was taken almost verbatim from the SSH library for Haskell.
https://hackage.haskell.org/package/hssh-0.1.0.0/docs/src/Network.SSH.Key.html
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.SSH.Private
    ( sshKeyPair
    , sshKeySelectors
    , KeyPair (..)
    , SSHClient (..)
    , Sign
    , sign
    , mkKeyAPI
    , WithSelector (..)
    , SelectorField
    , Selection (..)
    , mkKeyPair
    ) where

import Control.Applicative (many, (<|>))
import Control.Monad (replicateM, unless, void, when, (<=<))
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
    ( BlockCipher (blockSize, cbcDecrypt, ctrCombine)
    , Cipher (cipherInit, cipherKeySize)
    , KeySizeSpecifier (KeySizeFixed)
    , makeIV
    )
import Crypto.Error
    ( CryptoFailable (..)
    )
import Crypto.KDF.BCryptPBKDF qualified as BCryptPBKDF
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Binary.Get
    ( Decoder (..)
    , Get
    , getByteString
    , getRemainingLazyByteString
    , getWord32be
    , getWord8
    , isolate
    , pushChunk
    , runGet
    , runGetIncremental
    , runGetOrFail
    )
import Data.Bits (Bits (shiftL, shiftR, (.&.)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import Lib.JSON.Canonical.Extra (object, (.=))
import Lib.SSH.Public (SSHPublicKey, encodeSSHPublicKey)
import Text.JSON.Canonical (ToJSON (..))

data WithSelector = WithSelector | WithoutSelector

type family SelectorField (w :: WithSelector) where
    SelectorField 'WithSelector = Maybe String
    SelectorField 'WithoutSelector = ()

data SSHClient sel = SSHClient
    { sshKeySelector :: SelectorField sel
    , sshKeyFile :: FilePath
    , sshKeyPassphrase :: String
    }

deriving instance
    Show (SelectorField sel) => Show (SSHClient sel)
deriving instance
    Eq (SelectorField sel) => Eq (SSHClient sel)

type Sign = BC.ByteString -> Ed25519.Signature

sign :: KeyPair -> Sign
sign (KeyPair pk sk) = Ed25519.sign sk pk

mkKeyAPI :: String -> ByteString -> Maybe String -> Maybe KeyPair
mkKeyAPI passPhrase content sshKeySelector =
    let
        ks = decodePrivateKeyFile (BC.pack passPhrase) content
        mkMap (KeyPair pk sk, comment) =
            Map.singleton (BC.unpack comment)
                $ KeyPair
                    { publicKey = pk
                    , privateKey = sk
                    }
    in
        maybe (fmap snd <$> Map.lookupMin) Map.lookup sshKeySelector
            $ foldMap mkMap ks

sshKeyPair
    :: SSHClient 'WithSelector
    -> IO (Maybe KeyPair)
sshKeyPair SSHClient{sshKeySelector, sshKeyFile, sshKeyPassphrase} = do
    content <- B.readFile sshKeyFile
    pure $ mkKeyAPI sshKeyPassphrase content sshKeySelector

data Selection = Selection
    { selectorName :: String
    , selectorKey :: SSHPublicKey
    }
    deriving (Show, Eq)

instance Monad m => ToJSON m Selection where
    toJSON (Selection name key) =
        object
            [ "name" .= name
            , "publicKey" .= key
            ]

sshKeySelectors :: SSHClient 'WithoutSelector -> IO [Selection]
sshKeySelectors SSHClient{sshKeyFile, sshKeyPassphrase} = do
    content <- B.readFile sshKeyFile
    let ks = decodePrivateKeyFile (BC.pack sshKeyPassphrase) content
    let f (KeyPair k _, comment) = Selection (BC.unpack comment) render
          where
            render = encodeSSHPublicKey k
    pure $ fmap f ks

data KeyPair
    = KeyPair
    { publicKey :: Ed25519.PublicKey
    , privateKey :: Ed25519.SecretKey
    }
    deriving (Eq, Show)

mkKeyPair :: Ed25519.SecretKey -> KeyPair
mkKeyPair sk = KeyPair{publicKey = Ed25519.toPublic sk, privateKey = sk}

decodePrivateKeyFile
    :: ByteString
    -> ByteString
    -> [(KeyPair, ByteString)]
decodePrivateKeyFile passphrase =
    runGet (parsePrivateKeyFile passphrase) . BL.fromStrict

parsePrivateKeyFile
    :: ByteString
    -> Get [(KeyPair, ByteString)]
parsePrivateKeyFile passphrase = do
    bytes "-----BEGIN OPENSSH PRIVATE KEY-----"
    void $ many space
    keys <- feedingBase64 $ parseKeys passphrase
    void $ many space
    bytes "-----END OPENSSH PRIVATE KEY-----"
    void $ many space
    pure keys

fromDecoder :: Decoder a -> Get a
fromDecoder (Done _ _ a) = pure a
fromDecoder (Partial _f) = fail "Unexpected end of input"
fromDecoder (Fail _ _ err) = fail err

feedingBase64 :: Get a -> Get a
feedingBase64 = fromDecoder <=< s0 . runGetIncremental
  where
    continue d c = pure $ pushChunk d (B.pack c)
    b1 i j = (i `shiftL` 2) + (j `shiftR` 4)
    b2 j k = ((j .&. 15) `shiftL` 4) + (k `shiftR` 2)
    b3 k l = ((k .&. 3) `shiftL` 6) + l
    wc f = getWord8 >>= base64Value >>= f
    ws f = space1 >> f
    -- Initial state and final state.
    s0 xs = wc (s1 xs) <|> ws (s0 xs) <|> pure xs
    -- One character read (i). Three more characters or whitespace expected.
    s1 xs i = wc (s2 xs i) <|> ws (s1 xs i)
    -- Two characters read (i and j). Either '==' or space or two more character expected.
    s2 xs i j = r2 xs i j <|> wc (s3 xs i j) <|> ws (s2 xs i j)
    -- Three characters read (i, j and k). Either a '=' or space or one more character expected.
    s3 xs i j k = r3 xs i j k <|> wc (s4 xs i j k) <|> ws (s3 xs i j k)
    -- Four characters read (i, j, k and l). Computation of result and transition back to s0.
    s4 xs i j k l = continue xs [b1 i j, b2 j k, b3 k l] >>= s0
    -- Read two '=' chars as finalizer. Only valid from state s2.
    r2 xs i j = end >> end >> continue xs [b1 i j]
    -- Read one '=' char as finalizer. Only valid from state s1.
    r3 xs i j k = end >> continue xs [b1 i j, b2 j k]
    end = byte (== 61) -- '='

base64Value :: Word8 -> Get Word8
base64Value c
    | c == 43 = pure 62 -- '+'
    | c == 47 = pure 63 -- '/'
    | c >= 48 && c <= 57 = pure $ c - 48 + 52 -- '0'..'9'
    | c >= 65 && c <= 90 = pure $ c - 65 -- 'A'..'Z'
    | c >= 97 && c <= 122 = pure $ c - 97 + 26 -- 'a'..'z'
    | otherwise = fail "Invalid base64 character"

byte :: (Word8 -> Bool) -> Get ()
byte t = do
    c <- getWord8
    unless (t c)
        $ fail
        $ "Expected byte matching predicate, got " ++ show c

isSpace :: Word8 -> Bool
isSpace c = c == 32 || c == 10 || c == 13 || c == 9

space :: Get ()
space = byte isSpace

space1 :: Get ()
space1 = space >> many space >> pure ()

bytestring :: Get ByteString
bytestring = num >>= getByteString

num :: Num a => Get a
num = fromIntegral <$> getWord32be

bytes :: ByteString -> Get ()
bytes bs = do
    bs' <- getByteString $ B.length bs
    when (bs /= bs') $ failB $ "Expected " <> bs <> ", got " <> bs'

feedingDecrypted
    :: Get a -- to run on decrypted
    -> (ByteString -> Get ByteString) -- decryptor
    -> Get a
feedingDecrypted p f = do
    len <- num
    s <- isolate len $ rest >>= f
    case runGetOrFail p $ BL.fromStrict s of
        Left (_, _, err) -> fail err
        Right (_, _, a) -> pure a
  where
    rest :: Get B.StrictByteString
    rest = BL.toStrict <$> getRemainingLazyByteString

aes256T :: AES256
aes256T = undefined :: AES256

aes256CipherSize :: Int
aes256CipherSize = case cipherKeySize aes256T of
    KeySizeFixed keySize -> keySize
    _ -> error "Unsupported key size for AES-256"

a256IVSize :: Int
a256IVSize = blockSize aes256T

failB :: MonadFail m => B.ByteString -> m a
failB = fail . BC.unpack

-- https://github.com/openssh/openssh-portable/blob/master/PROTOCOL.key

parseKeys
    :: ()
    => ByteString
    -> Get [(KeyPair, ByteString)]
parseKeys passphrase = do
    bytes "openssh-key-v1\NUL"
    cipherAlgo <- bytestring
    kdfAlgo <- bytestring
    keyAndIV <- selfIsolate $ \_ -> case kdfAlgo of
        "bcrypt" -> do
            salt <- bytestring
            rounds <- num
            let len = aes256CipherSize + a256IVSize
            pure
                $ BCryptPBKDF.generate
                    (BCryptPBKDF.Parameters rounds len)
                    passphrase
                    salt
        _ -> failB $ "Unsupported key derivation function " <> kdfAlgo
    let (key, ivBytes) = B.splitAt aes256CipherSize keyAndIV
    cipher <- cryptoFail $ cipherInit @AES256 key
    iv <- case makeIV ivBytes of
        Nothing -> fail "Invalid IV size"
        Just iv' -> pure iv'
    numberOfKeys <- num
    _publicKeysRaw <- bytestring
    feedingDecrypted (parsePrivateKeys numberOfKeys) $ case cipherAlgo of
        "none" -> pure
        "aes256-cbc" -> pure . cbcDecrypt cipher iv
        "aes256-ctr" -> pure . ctrCombine cipher iv
        _ -> const $ failB $ "Unsupported cipher algorithm " <> cipherAlgo

cryptoFail :: MonadFail m => CryptoFailable a -> m a
cryptoFail (CryptoPassed a) = pure a
cryptoFail (CryptoFailed e) = fail $ show e

selfIsolate :: (Int -> Get a) -> Get a
selfIsolate p = do
    len <- num
    isolate len $ p len

parsePrivateKeys
    :: Int
    -> Get [(KeyPair, ByteString)]
parsePrivateKeys count = do
    check1 <- getWord32be
    check2 <- getWord32be
    when (check1 /= check2) $ fail "Unsuccessful decryption"
    replicateM count $ do
        algo <- bytestring
        key <- case algo of
            "ssh-ed25519" -> do
                _pkCopy <- bytestring -- public key copy
                selfIsolate $ \_ -> do
                    secretKeyRaw <- getByteString Ed25519.secretKeySize
                    publicKeyRaw <- getByteString Ed25519.publicKeySize
                    cryptoFail
                        $ KeyPair
                            <$> Ed25519.publicKey publicKeyRaw
                            <*> Ed25519.secretKey secretKeyRaw
            _ -> failB $ "Unsupported algorithm for private key " <> algo
        comment <- bytestring
        pure (key, comment)
