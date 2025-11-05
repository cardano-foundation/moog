module Core.Types.VKeySpec
    ( spec
    )
where

import Core.Types.VKey (decodeVKey, encodeVKey)
import Crypto.PubKey.Ed25519 (PublicKey)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (forAllBlind)
import Test.QuickCheck.Crypton (ed25519Gen)

-- Roundtrip property: encoding and then decoding should yield the original value
prop_roundtripVKey :: PublicKey -> Bool
prop_roundtripVKey pkey = case encodeVKey pkey of
    Left _ -> False
    Right vkey -> case decodeVKey vkey of
        Left _ -> False
        Right pkey' -> pkey == pkey'

spec :: Spec
spec = do
    describe "VKey JSON encoding" $ do
        it "should roundtrip successfully"
            $ forAllBlind ed25519Gen
            $ \(_, pkey) ->
                prop_roundtripVKey pkey
