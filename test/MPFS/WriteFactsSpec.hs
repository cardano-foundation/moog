module MPFS.WriteFactsSpec (spec) where

import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( DeleteRequest (..)
    , InsertRequest (..)
    , RetractRequest (..)
    , UpdateRequest (..)
    , UpdateValueRequest (..)
    )
import Cardano.MPFS.API.Types.Common (TokenIdJSON (..))
import Codec.Binary.Bech32 qualified as Bech32
import Core.Types.Basic
    ( Address (..)
    , RequestRefId (..)
    , TokenId (..)
    )
import Core.Types.Tx
    ( UnsignedTx (..)
    , WithUnsignedTx (..)
    )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Identity (Identity (..))
import Data.List (isPrefixOf)
import MPFS.API
    ( MPFS (..)
    , RequestDeleteBody (..)
    , RequestInsertBody (..)
    , RequestUpdateBody (..)
    )
import MPFS.Request
    ( requestDeleteFactsRequest
    , requestInsertFactsRequest
    , requestUpdateFactsRequest
    )
import MPFS.Retract (retractFactsRequest)
import MPFS.Update (updateTokenFactsRequest)
import MockMPFS (mockMPFS)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import Text.JSON.Canonical
    ( JSValue (..)
    , renderCanonicalJSON
    , toJSString
    )

spec :: Spec
spec =
    describe "facts-based MPFS write requests" $ do
        it "builds POST /facts/request/insert request bodies" $ do
            let body = RequestInsertBody sampleKey sampleNewValue

            case requestInsertFactsRequest sampleAddress sampleToken body of
                Left err -> error err
                Right InsertRequest{..} -> do
                    irToken `shouldBe` sampleTokenJSON
                    irKey `shouldBe` canonicalHex sampleKey
                    irValue `shouldBe` canonicalHex sampleNewValue
                    irAddr `shouldBe` Hex sampleAddressBytes

        it "builds POST /facts/request/delete request bodies" $ do
            let body = RequestDeleteBody sampleKey sampleOldValue

            case requestDeleteFactsRequest sampleAddress sampleToken body of
                Left err -> error err
                Right DeleteRequest{..} -> do
                    drToken `shouldBe` sampleTokenJSON
                    drKey `shouldBe` canonicalHex sampleKey
                    drValue `shouldBe` canonicalHex sampleOldValue
                    drAddr `shouldBe` Hex sampleAddressBytes

        it "builds POST /facts/request/update request bodies" $ do
            let body = RequestUpdateBody sampleKey sampleOldValue sampleNewValue

            case requestUpdateFactsRequest sampleAddress sampleToken body of
                Left err -> error err
                Right UpdateValueRequest{..} -> do
                    uvrToken `shouldBe` sampleTokenJSON
                    uvrKey `shouldBe` canonicalHex sampleKey
                    uvrOldValue `shouldBe` canonicalHex sampleOldValue
                    uvrNewValue `shouldBe` canonicalHex sampleNewValue
                    uvrAddr `shouldBe` Hex sampleAddressBytes

        it "builds POST /facts/update request bodies" $
            case updateTokenFactsRequest sampleAddress sampleToken of
                Left err -> error err
                Right UpdateRequest{..} -> do
                    urToken `shouldBe` sampleTokenJSON
                    urAddr `shouldBe` Hex sampleAddressBytes

        it "builds POST /facts/retract request bodies" $
            case retractFactsRequest sampleAddress sampleRequestRef of
                Left err -> error err
                Right RetractRequest{..} -> do
                    rrUtxo `shouldBe` requestId sampleRequestRef
                    rrAddr `shouldBe` Hex sampleAddressBytes

        it "fails closed when token ids are not hex" $
            case requestInsertFactsRequest
                sampleAddress
                (TokenId "not hex")
                (RequestInsertBody sampleKey sampleNewValue) of
                Left err ->
                    err `shouldSatisfy` ("invalid token id hex:" `isPrefixOf`)
                Right _ -> error "expected malformed token id to fail"

        it "adds facts-based write operations to the MPFS record" $ do
            let tx = WithUnsignedTx (UnsignedTx "00") Nothing
                mpfs =
                    mockMPFS
                        { mpfsRequestInsertFromFacts =
                            \_ _ _ -> Identity tx
                        , mpfsRequestDeleteFromFacts =
                            \_ _ _ -> Identity tx
                        , mpfsRequestUpdateFromFacts =
                            \_ _ _ -> Identity tx
                        , mpfsUpdateTokenFromFacts =
                            \_ _ -> Identity tx
                        , mpfsRetractChangeFromFacts =
                            \_ _ -> Identity tx
                        }

            runIdentity
                ( mpfsUpdateTokenFromFacts
                    mpfs
                    sampleAddress
                    sampleToken
                )
                `shouldBe` tx

sampleAddress :: Address
sampleAddress =
    Address
        $ case Bech32.humanReadablePartFromText "addr_test" of
            Left err -> error $ show err
            Right hrp ->
                case Bech32.encode
                    hrp
                    (Bech32.dataPartFromBytes sampleAddressBytes) of
                    Left err -> error $ show err
                    Right encoded -> encoded

sampleAddressBytes :: BS.ByteString
sampleAddressBytes =
    BS.pack [0x00 .. 0x1c]

sampleToken :: TokenId
sampleToken =
    TokenId "abcd"

sampleTokenJSON :: TokenIdJSON
sampleTokenJSON =
    TokenIdJSON "\xab\xcd"

sampleRequestRef :: RequestRefId
sampleRequestRef =
    RequestRefId "0123456789abcdef#0"

sampleKey :: JSValue
sampleKey =
    JSString $ toJSString "key"

sampleOldValue :: JSValue
sampleOldValue =
    JSString $ toJSString "old"

sampleNewValue :: JSValue
sampleNewValue =
    JSString $ toJSString "new"

canonicalHex :: JSValue -> Hex
canonicalHex =
    Hex . BL.toStrict . renderCanonicalJSON
