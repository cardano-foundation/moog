module MPFS.ReadSpec (spec) where

import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types (FactEntry (..), FactsResponse (..))
import Cardano.MPFS.Client.TrustedRoot (TrustedRoot (..))
import Core.Types.Fact (Fact (..), Slot (..), parseFacts)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import MPFS.Read
    ( factEntriesToJSValue
    , verifyFactsResponseWith
    )
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
    describe "verified MPFS facts reads" $ do
        it "maps verified FactEntry values to moog facts JSON" $ do
            parseFacts (factEntriesToJSValue sampleEntries)
                `shouldBe`
                [ Fact sampleKey sampleValue (Slot 0)
                ]

        it "fails closed when verification rejects the response" $ do
            let result =
                    verifyFactsResponseWith
                        (\_ _ -> Left SampleVerifyError)
                        sampleTrustedRoot
                        unusedFactsResponse

            result `shouldSatisfy` isLeft

sampleEntries :: [FactEntry]
sampleEntries =
    [ FactEntry
        { feKey = Hex $ canonicalBytes sampleKey
        , feValue = Hex $ canonicalBytes sampleValue
        }
    ]

sampleKey :: JSValue
sampleKey =
    JSString $ toJSString "key"

sampleValue :: JSValue
sampleValue =
    JSString $ toJSString "value"

canonicalBytes :: JSValue -> BS.ByteString
canonicalBytes =
    BL.toStrict . renderCanonicalJSON

sampleTrustedRoot :: TrustedRoot
sampleTrustedRoot =
    TrustedRoot $ Hex ""

unusedFactsResponse :: FactsResponse
unusedFactsResponse =
    FactsResponse
        { frsSnapshot = error "unused snapshot"
        , frsState = error "unused state"
        , frsFacts = []
        }

data SampleVerifyError = SampleVerifyError
    deriving stock (Show)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
