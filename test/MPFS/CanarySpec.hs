module MPFS.CanarySpec (spec) where

import Control.Exception (throwIO)
import Core.Types.Basic (TokenId (..))
import Core.Types.Tx (SignedTx (..), TxHash (..))
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IORef
import MPFS.API (submitRequestFromSignedTx)
import MPFS.Canary
    ( BootCanaryFailure (..)
    , FullLifecycleCanaryResult (..)
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
                encode (submitRequestFromSignedTx (SignedTx "deadbeef"))
                    `shouldBe` BSL.pack
                        "{\"signedTxCbor\":\"deadbeef\"}"

        describe "full lifecycle result" $ do
            it "identifies the write-submit lifecycle observations" $ do
                value <-
                    toJSON
                        FullLifecycleCanaryResult
                            { canaryBootTxHash = TxHash "boot"
                            , canaryRequestInsertTxHash =
                                TxHash "request-insert"
                            , canaryUpdateTokenTxHash =
                                TxHash "update-token"
                            , canaryEndTxHash = TxHash "end"
                            , canaryTokenId = TokenId "abcd"
                            , bootTransactionPolls = 1
                            , requestInsertTransactionPolls = 2
                            , requestObservedPolls = 3
                            , updateTokenTransactionPolls = 4
                            , factObservedPolls = 5
                            , tokenBeforeEndPolls = 6
                            , endTransactionPolls = 7
                            , tokenGonePolls = 8
                            }
                renderCanonicalJSON value
                    `shouldBe` BSL.pack
                        "{\"bootTransactionObserved\":true,\"bootTransactionPolls\":1,\"bootTxHash\":\"boot\",\"boundary\":\"mpfs-v2-full-lifecycle\",\"endTransactionObserved\":true,\"endTransactionPolls\":7,\"endTxHash\":\"end\",\"factObserved\":true,\"factObservedPolls\":5,\"requestInsertTransactionObserved\":true,\"requestInsertTransactionPolls\":2,\"requestInsertTxHash\":\"request-insert\",\"requestObserved\":true,\"requestObservedPolls\":3,\"tokenBeforeEndPolls\":6,\"tokenGoneObserved\":true,\"tokenGonePolls\":8,\"tokenId\":\"abcd\",\"tokenObservedBeforeEnd\":true,\"updateTokenTransactionObserved\":true,\"updateTokenTransactionPolls\":4,\"updateTokenTxHash\":\"update-token\"}"

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
