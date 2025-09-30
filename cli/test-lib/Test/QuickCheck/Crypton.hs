{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.QuickCheck.Crypton
    ( ed25519Gen
    )
where

import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 (generateSecretKey)
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Crypto.Random (MonadRandom (..))
import Data.ByteArray qualified as BA
import Data.ByteString.Char8 qualified as B
import Data.Word (Word8)
import Test.QuickCheck
    ( Gen
    , choose
    , vectorOf
    )

instance MonadRandom Gen where
    getRandomBytes n = do
        bytes <- vectorOf n (choose (0, 255) :: Gen Word8)
        return $ BA.pack bytes

ed25519Gen
    :: Gen
        ( String -> Ed25519.Signature
        , Ed25519.PublicKey
        )
ed25519Gen = do
    private <- generateSecretKey
    let public = Ed25519.toPublic private
        sign message =
            let signature = Ed25519.sign private public $ B.pack message
            in  case Ed25519.signature signature of
                    CryptoFailed err -> error $ "Invalid signature: " ++ show err
                    CryptoPassed sig -> sig
    return (sign, public)
