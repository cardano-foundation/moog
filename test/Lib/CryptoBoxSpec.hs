{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.CryptoBoxSpec
    ( spec
    )
where

import Crypto.PubKey.Ed25519
    ( PublicKey
    , SecretKey
    , generateSecretKey
    , toPublic
    )
import Crypto.Random (MonadRandom (..))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Functor ((<&>))
import Data.Word (Word8)
import Lib.CryptoBox
    ( Nonce192 (..)
    , decryptOnly
    , encryptAndSign
    , encryptOnly
    , verifiyAndDecrypt
    )
import Test.Hspec (Spec, describe, expectationFailure, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , forAllBlind
    , listOf
    , vectorOf
    , withMaxSuccess
    )

data Params = Params
    { agentPk :: PublicKey
    , agentSk :: SecretKey
    , userPk :: PublicKey
    , userSk :: SecretKey
    , msg :: ByteString
    , nonce :: Nonce192
    }

instance MonadRandom Gen where
    getRandomBytes n = convert . B.pack <$> vectorOf n (arbitrary :: Gen Word8)

generateParams :: Gen Params
generateParams = do
    agentSk <- generateSecretKey
    let agentPk = toPublic agentSk
    userSk <- generateSecretKey
    let userPk = toPublic userSk
    msg <- listOf (arbitrary :: Gen Word8) <&> B.pack
    nonce <- vectorOf (192 `div` 8) (arbitrary :: Gen Word8) <&> B.pack
    pure
        $ Params{agentPk, agentSk, userPk, userSk, msg, nonce = Nonce192 nonce}

spec :: Spec
spec = describe "CryptoBox"
    $ do
        prop "can create and open"
            $ withMaxSuccess 10000
            $ forAllBlind generateParams
            $ \Params{agentSk, agentPk, userPk, userSk, msg, nonce} ->
                case encryptAndSign userPk agentSk msg nonce of
                    Left err -> expectationFailure err
                    Right closed ->
                        case verifiyAndDecrypt agentPk userSk closed nonce of
                            Left err -> expectationFailure err
                            Right opened -> opened `shouldBe` Just msg
        prop "can encrypt and decrypt only"
            $ withMaxSuccess 10000
            $ forAllBlind generateParams
            $ \Params{userPk, userSk, msg, nonce} ->
                case encryptOnly userPk msg nonce of
                    Left err -> expectationFailure err
                    Right encrypted ->
                        case decryptOnly userSk encrypted nonce of
                            Left err -> expectationFailure err
                            Right decrypted -> decrypted `shouldBe` Just msg
