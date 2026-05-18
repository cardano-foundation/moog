module MPFS.CanarySpec (spec) where

import Control.Exception (throwIO)
import Core.Types.Basic (TokenId (..))
import Core.Types.Tx (SignedTx (..), TxHash (..))
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IORef
import MPFS.API (SubmitV2Body (..))
import MPFS.Canary
    ( BootCanaryFailure (..)
    , EndBoundaryCanaryResult (..)
    , pollGoneBoundaryWithDelay
    , tokenIdFromBootValue
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldReturn
    )
import Text.JSON.Canonical
    ( JSValue (..)
    , renderCanonicalJSON
    , toJSON
    , toJSString
    )

spec :: Spec
spec =
    describe "MPFS v2 boundary canary" $ do
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

        describe "end boundary result" $ do
            it "identifies the boot-then-end observations" $ do
                value <-
                    toJSON
                        EndBoundaryCanaryResult
                            { canaryBootTxHash = TxHash "boot"
                            , canaryEndTxHash = TxHash "end"
                            , canaryTokenId = TokenId "abcd"
                            , bootTransactionPolls = 1
                            , tokenBeforeEndPolls = 2
                            , endTransactionPolls = 3
                            , tokenGonePolls = 4
                            }
                renderCanonicalJSON value
                    `shouldBe` BSL.pack
                        "{\"bootTransactionObserved\":true,\"bootTransactionPolls\":1,\"bootTxHash\":\"boot\",\"boundary\":\"mpfs-v2-end\",\"endTransactionObserved\":true,\"endTransactionPolls\":3,\"endTxHash\":\"end\",\"tokenBeforeEndPolls\":2,\"tokenGoneObserved\":true,\"tokenGonePolls\":4,\"tokenId\":\"abcd\",\"tokenObservedBeforeEnd\":true}"

        describe "token-gone polling" $ do
            it
                "returns the number of successful token observations before lookup fails"
                $ do
                    attempts <- newIORef (0 :: Int)
                    pollGoneBoundaryWithDelay
                        "token gone"
                        5
                        (pure ())
                        ( do
                            attempt <- readIORef attempts
                            writeIORef attempts (attempt + 1)
                            if attempt < 2
                                then pure ()
                                else throwIO $ userError "404 token not found"
                        )
                        `shouldReturn` 2
