module MPFS.CanarySpec (spec) where

import Core.Types.Basic (TokenId (..))
import Core.Types.Tx (SignedTx (..))
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BSL
import MPFS.API (SubmitV2Body (..))
import MPFS.Canary
    ( BootCanaryFailure (..)
    , tokenIdFromBootValue
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.JSON.Canonical (JSValue (..), toJSString)

spec :: Spec
spec =
    describe "boot canary" $ do
        describe "token id parsing" $ do
            it "accepts the token id returned by the boot facts path" $ do
                tokenIdFromBootValue
                    (Just $ JSString $ toJSString "abcd")
                    `shouldBe` Right (TokenId "abcd")

            it "fails loudly when boot returns no token id" $ do
                tokenIdFromBootValue Nothing
                    `shouldBe` Left NoBootTokenIdReturned

            it "fails loudly when the token id is not canonical text" $ do
                tokenIdFromBootValue (Just JSNull)
                    `shouldBe` Left BootTokenIdNotValidJSON

        describe "MPFS v2 submit body" $ do
            it "uses the POST /tx/submit wire key" $ do
                encode (SubmitV2Body (SignedTx "deadbeef"))
                    `shouldBe` BSL.pack "{\"tx\":\"deadbeef\"}"
