module Lib.JSONSpec
    ( spec
    )
where

import Control.Monad ((>=>))
import Data.Aeson.Types (parseMaybe)
import Lib.JSON
    ( CanonicalJSONError (CanonicalJSONError)
    , fromAesonString
    , runCanonicalJSON
    , runIdentityCanonicalJSON
    , toAesonString
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.JSON.Canonical
    ( FromJSON (..)
    , Int54
    , JSValue (..)
    , ToJSON (..)
    )

spec :: Spec
spec = do
    describe "CanonicalJSON monad" $ do
        it "can be run on Identity" $ do
            let result = runIdentityCanonicalJSON $ toJSON JSNull
            result `shouldBe` Right JSNull
        it "can be run on IO" $ do
            result <- runCanonicalJSON $ toJSON (JSString "test")
            result `shouldBe` Right (JSString "test")
        it "catches errors" $ do
            result :: Either CanonicalJSONError Int54 <-
                runCanonicalJSON $ fromJSON (JSObject mempty)
            result
                `shouldBe` Left
                    (CanonicalJSONError "int" $ Just "object")
    describe "fromAesonString and toAesonString" $ do
        it "converts JSNull" $ do
            let jsValue = JSNull
            let str = toAesonString jsValue
            parseMaybe fromAesonString str `shouldBe` Just jsValue
        it "encode ()" $ do
            let value = ()
            toJSON value `shouldBe` Just JSNull
            (toJSON >=> fromJSON) value `shouldBe` Just ()
