{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}
module Core.Types.DurationSpec
    ( spec
    )
where

import Core.Types.Duration
    ( Duration (..)
    , durationToTimeDiff
    )
import Oracle.Validate.Requests.TestRun.Lib (genDuration)
import System.Time (TimeDiff (tdHour, tdMin))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Canonical (roundTrip)
import Test.QuickCheck (forAll)

spec :: Spec
spec = describe "Duration" $ do
    it "converts Hours to TimeDiff correctly" $ do
        let duration = Hours 5
            timeDiff = durationToTimeDiff duration
        tdHour timeDiff `shouldBe` 5
        tdMin timeDiff `shouldBe` 0
    it "converts Minutes to TimeDiff correctly" $ do
        let duration = Minutes 30
            timeDiff = durationToTimeDiff duration
        tdHour timeDiff `shouldBe` 0
        tdMin timeDiff `shouldBe` 30
    it "roundtrips on the JSON instance"
        $ forAll genDuration
        $ roundTrip @Duration
    it "can compare Durations correctly" $ do
        Minutes 0 `shouldBe` Hours 0
        Minutes 60 `shouldBe` Hours 1
        Minutes 90 `shouldBe` Hours 1 `mappend` Minutes 30
        Hours 2 `shouldBe` Minutes 120
        Hours 1 < Minutes 90 `shouldBe` True
        Minutes 45 < Hours 1 `shouldBe` True
        Hours 3 > Minutes 150 `shouldBe` True
        Minutes 30 > Hours 1 `shouldBe` False
    it "is a semigroup" $ forAll genDuration $ \d1 ->
        forAll genDuration $ \d2 ->
            forAll genDuration $ \d3 ->
                (d1 `mappend` (d2 `mappend` d3))
                    `shouldBe` ((d1 `mappend` d2) `mappend` d3)
    it "is a monoid" $ forAll genDuration $ \d -> do
        (d `mappend` mempty) `shouldBe` d
        (mempty `mappend` d) `shouldBe` d
