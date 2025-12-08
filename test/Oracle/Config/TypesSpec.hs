module Oracle.Config.TypesSpec
    ( spec
    , genConfig
    )
where

import Core.Types.Basic (Owner (..))
import Oracle.Config.Types (Config (..))
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Requests.TestRun.Lib (genDuration)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Canonical (roundTrip)
import Test.QuickCheck
    ( Gen
    , Testable (..)
    , forAll
    , suchThat
    )
import Test.QuickCheck.JSString (genAscii)

genConfig :: Gen Config
genConfig = do
    agent <- Owner <$> genAscii
    minDuration <- genDuration `suchThat` (> mempty)
    maxDuration <- genDuration `suchThat` (> minDuration)
    let testRunValidationConfig =
            TestRunValidationConfig
                { minDuration = minDuration
                , maxDuration = maxDuration
                }
    return $ Config agent testRunValidationConfig

spec :: Spec
spec = do
    describe "WhiteListKey" $ do
        it "roundtrips on the JSON instance"
            $ property
            $ forAll genConfig
            $ \key ->
                roundTrip key
