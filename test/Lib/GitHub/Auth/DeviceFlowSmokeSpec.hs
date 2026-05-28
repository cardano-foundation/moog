{-# LANGUAGE OverloadedStrings #-}

module Lib.GitHub.Auth.DeviceFlowSmokeSpec
    ( spec
    )
where

import Data.Text qualified as T
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Moog.GitHub.DeviceFlowSmoke (renderTokenEvidence)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
    describe "renderTokenEvidence" $ do
        it "prints the token prefix and length without printing the full token" $ do
            let token = "ghu_abcdefghijklmnopqrstuvwxyz"
                evidence = renderTokenEvidence $ OAuthToken token

            evidence `shouldSatisfy` T.isInfixOf "ghu_"
            evidence `shouldSatisfy` T.isInfixOf "30"
            evidence `shouldSatisfy` not . T.isInfixOf "ghu_abcdefghijklmnopqrstuvwxyz"
            T.length evidence `shouldBe` 13
