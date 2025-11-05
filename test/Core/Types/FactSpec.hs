module Core.Types.FactSpec (spec) where

import Core.Types.Fact (Fact (..), Slot (..), parseFacts, renderFacts)
import Data.List (sort)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.Hspec.Canonical (roundTrip)
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , forAll
    , forAllShrink
    , getSize
    , suchThat
    )
import Test.QuickCheck.JSString (genAscii)
import Text.JSON.Canonical
    ( FromJSON (..)
    , ReportSchemaErrors
    , ToJSON (..)
    )

spec :: Spec
spec = describe "Core.Types.Fact" $ do
    it "roundTrip JSON serialization of Fact"
        $ forAll genFacts
        $ roundTrip @(Fact K V)
    it "roundTrip JSON API serialization of Fact"
        $ forAllShrink genUniqueFacts (pure . drop 1)
        $ \fs -> sort fs `shouldBe` sort (parseFacts (renderFacts fs))

genUniqueFacts :: Gen [Fact K V]
genUniqueFacts = do
    size <- getSize
    let go _ 0 = pure []
        go seen n = do
            k <- genAscii `suchThat` (`notElem` seen)
            v <- genAscii
            slot <- Slot <$> arbitrary
            let fact = Fact (K k) (V v) slot
            rest <- go (k : seen) (n - 1)
            pure (fact : rest)
    go [] size

genFacts :: Gen (Fact K V)
genFacts = do
    k <- genAscii
    v <- genAscii
    Fact (K k) (V v) . Slot <$> arbitrary

newtype K = K String deriving (Eq, Show, Ord)

instance Monad m => ToJSON m K where
    toJSON (K s) = toJSON s

instance ReportSchemaErrors m => FromJSON m K where
    fromJSON v = K <$> fromJSON v

newtype V = V String deriving (Eq, Show, Ord)

instance Monad m => ToJSON m V where
    toJSON (V i) = toJSON i

instance ReportSchemaErrors m => FromJSON m V where
    fromJSON v = V <$> fromJSON v
