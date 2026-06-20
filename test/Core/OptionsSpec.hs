{-# LANGUAGE OverloadedStrings #-}

module Core.OptionsSpec
    ( spec
    ) where

import Core.Options (parseOutputReference)
import Core.Types.Basic (RequestRefId (..))
import Data.Text qualified as T
import OptEnvConf.Reader (Reader (..))
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

spec :: Spec
spec =
    describe "Core.Options" $ do
        describe "parseOutputReference" $ do
            it "accepts v2 txHash#index output references" $ do
                runParse outputReference
                    `shouldBe` Right (RequestRefId $ T.pack outputReference)

            it "normalizes legacy txHash-index output references to txHash#index" $ do
                runParse legacyOutputReference
                    `shouldBe` Right (RequestRefId $ T.pack outputReference)

            it "rejects output references with non-integer indexes" $ do
                runParse (txHash <> "#x")
                    `shouldBe` Left
                        "Invalid index format. Use 'txHash#index' where index is an integer."

runParse :: String -> Either String RequestRefId
runParse input =
    let Reader parse = parseOutputReference
    in  parse input

outputReference :: String
outputReference = txHash <> "#0"

legacyOutputReference :: String
legacyOutputReference = txHash <> "-0"

txHash :: String
txHash = "a9ffa9ffa9ffa9ffa9ffa9ffa9ffa9ffa9ffa9ffa9ffa9ffa9ffa9ffa9ff193a"
