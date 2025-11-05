module Test.Hspec.Canonical
    ( roundTrip
    )
where

import Test.Hspec (shouldBe)
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))

roundTrip
    :: (ToJSON Maybe a, FromJSON Maybe a, Show a, Eq a) => a -> IO ()
roundTrip value = do
    let decoded = toJSON value >>= fromJSON
    decoded `shouldBe` Just value
