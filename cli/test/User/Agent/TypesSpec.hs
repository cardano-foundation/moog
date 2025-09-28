module User.Agent.TypesSpec
    ( spec
    , genWhiteListKey
    , genRepository
    )
where

import Core.Types.Basic
    ( GithubRepository (..)
    , Platform (Platform)
    )
import Data.Char (isAscii)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Canonical (roundTrip)
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Testable (..)
    , forAll
    , suchThat
    )
import Test.QuickCheck.JSString (genAscii)
import User.Agent.Types (WhiteListKey (..))

genRepository :: Gen GithubRepository
genRepository = do
    owner <- genAscii
    GithubRepository owner <$> genAscii

genWhiteListKey :: Gen WhiteListKey
genWhiteListKey = do
    platform <- arbitrary `suchThat` all isAscii
    WhiteListKey (Platform platform) <$> genRepository

spec :: Spec
spec = do
    describe "WhiteListKey" $ do
        it "roundtrips on the JSON instance"
            $ property
            $ forAll genWhiteListKey
            $ \key ->
                roundTrip key
