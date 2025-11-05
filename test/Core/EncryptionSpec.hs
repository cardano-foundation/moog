module Core.EncryptionSpec
    ( spec
    )
where

import Core.Encryption
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck

gen :: Gen (ByteString, ByteString, Word8)
gen = do
    message <- B.pack <$> listOf arbitrary
    passphrase <- B.pack <$> listOf arbitrary
    runs <- choose (1, 20)
    pure (message, passphrase, runs)

genText :: Gen (Text, Text, Word8)
genText = do
    message <- T.pack <$> listOf arbitrary
    passphrase <- T.pack <$> listOf arbitrary
    runs <- choose (1, 20)
    pure (message, passphrase, runs)

spec :: Spec
spec = do
    describe "Encryption and Decryption" $ do
        it "should encrypt and decrypt a ByteString message correctly"
            $ forAll gen
            $ \(message, passphrase, runs) -> do
                encrypted <- encrypt passphrase runs message
                let decrypted = decrypt passphrase encrypted
                decrypted `shouldBe` Right message
        it "should encrypt and decrypt a Text message correctly"
            $ forAll genText
            $ \(message, passphrase, runs) -> do
                encrypted <- encryptText passphrase runs message
                let decrypted = decryptText passphrase encrypted
                decrypted `shouldBe` Right message
