module MPFS.BootSpec (spec) where

import Codec.Binary.Bech32 qualified as Bech32
import Core.Types.Basic (Address (..))
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as T
import MPFS.Boot (addressBytesForBoot)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
    describe "addressBytesForBoot" $ do
        it "decodes a Bech32 wallet address to raw address bytes" $ do
            let expected = BS.pack [0x00 .. 0x1c]
                address =
                    case Bech32.humanReadablePartFromText "addr_test" of
                        Left err -> error $ show err
                        Right hrp ->
                            case Bech32.encode
                                hrp
                                (Bech32.dataPartFromBytes expected) of
                                Left err -> error $ show err
                                Right encoded -> encoded

            addressBytesForBoot (Address address)
                `shouldBe` Right expected
            expected `shouldSatisfy` not . BS.null
            expected `shouldSatisfy` (/= T.encodeUtf8 address)
