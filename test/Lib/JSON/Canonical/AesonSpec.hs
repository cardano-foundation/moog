module Lib.JSON.Canonical.AesonSpec (spec) where

import Data.List (nub, sort)
import Data.Scientific (scientific)
import Lib.JSON.Canonical.Aeson (fromAeson, fromCanon, fromScientific)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , arbitraryASCIIChar
    , arbitrarySizedIntegral
    , forAll
    , listOf
    , oneof
    , scale
    , sized
    )
import Text.JSON.Canonical qualified as Canon

spec :: Spec
spec = do
    describe "Canonical JSON <-> Aeson conversions" $ do
        prop "Converts Canonical JSON to Aeson and back"
            $ forAll rCanon prop_roundTrip

    describe "Convert from Data.Scientific to Canonical JSNum" $ do
        it "Converts number with 0 exponent" $ do
            fromScientific (scientific 1234 0)
                `shouldBe` Just (Canon.JSNum 1234)

        it "Converts number with positive exponent" $ do
            fromScientific (scientific 12 2)
                `shouldBe` Just (Canon.JSNum 1200)

        it "Converts number with negative exponent that is integer" $ do
            fromScientific (scientific 1200 (-2))
                `shouldBe` Just (Canon.JSNum 12)

        it "Rejects number with negative exponent that is decimal" $ do
            fromScientific (scientific 1200 (-3))
                `shouldBe` Nothing

prop_roundTrip :: Canon.JSValue -> Bool
prop_roundTrip x = fromAeson (fromCanon x) == Just x

rCanon :: Gen Canon.JSValue
rCanon = sized rCanon'

rCanon' :: Int -> Gen Canon.JSValue
rCanon' n
    | n > 1 =
        oneof
            [ rCanon' 0
            , Canon.JSArray <$> listOf subCanon
            , sized rObject
            ]
    | otherwise =
        oneof
            [ return Canon.JSNull
            , Canon.JSBool <$> arbitrary
            , Canon.JSNum <$> rInt
            , Canon.JSString <$> rString
            ]
  where
    subCanon = rCanon' (n `div` 3)

rInt :: Gen Canon.Int54
rInt = arbitrarySizedIntegral

rString :: Gen Canon.JSString
rString = fmap Canon.toJSString (listOf arbitraryASCIIChar)

rObject :: Int -> Gen Canon.JSValue
rObject n
    | n > 1 = scale (div 3) $ zipObj <$> listOf rString <*> listOf rCanon
    | otherwise = return (Canon.JSObject [])

zipObj :: [Canon.JSString] -> [Canon.JSValue] -> Canon.JSValue
zipObj ss vs = Canon.JSObject (zip (nub $ sort ss) vs)
