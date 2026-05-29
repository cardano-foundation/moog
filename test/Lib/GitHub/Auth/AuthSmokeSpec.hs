{-# LANGUAGE OverloadedStrings #-}

module Lib.GitHub.Auth.AuthSmokeSpec
    ( spec
    )
where

import Data.Text qualified as T
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Lib.GitHub.Auth.Identify (GitHubError (..), Login (..))
import Lib.GitHub.Auth.TeamCheck
    ( MembershipResult (..)
    , Org (..)
    , TeamSlug (..)
    )
import Moog.GitHub.AuthSmoke
    ( SmokeConfig (..)
    , parseSmokeArgs
    , renderLoginCheck
    , renderMembershipCheck
    , renderTokenEvidence
    , tokenFromEnv
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

spec :: Spec
spec = do
    describe "renderTokenEvidence" $ do
        it "shows the token prefix and length without the full token" $ do
            let token = "ghp_abcdefghijklmnopqrstuvwxyz"
                evidence = renderTokenEvidence $ OAuthToken token
            evidence `shouldSatisfy` T.isInfixOf "ghp_"
            evidence `shouldSatisfy` T.isInfixOf "30"
            evidence
                `shouldSatisfy` not
                . T.isInfixOf "ghp_abcdefghijklmnopqrstuvwxyz"

    describe "tokenFromEnv" $ do
        it "fails closed when the variable is unset" $ do
            tokenFromEnv Nothing
                `shouldSatisfy` either
                    (T.isInfixOf "MOOG_GITHUB_OAUTH_TOKEN")
                    (const False)

        it "fails closed when the variable is empty" $ do
            tokenFromEnv (Just "")
                `shouldSatisfy` either (const True) (const False)

        it "accepts a non-empty token" $ do
            tokenFromEnv (Just "ghp_secret")
                `shouldBe` Right (OAuthToken "ghp_secret")

        it "never echoes the token bytes in the error path" $ do
            tokenFromEnv Nothing
                `shouldSatisfy` either
                    (not . T.isInfixOf "ghp_")
                    (const False)

    describe "parseSmokeArgs" $ do
        it "defaults org and team to pragma-org/antithesis-access" $ do
            parseSmokeArgs ["--expected-login", "alice"]
                `shouldBe` Right
                    SmokeConfig
                        { expectedLogin = Login "alice"
                        , org = Org "pragma-org"
                        , team = TeamSlug "antithesis-access"
                        }

        it "accepts explicit org and team overrides" $ do
            parseSmokeArgs
                [ "--expected-login"
                , "alice"
                , "--org"
                , "acme"
                , "--team"
                , "core"
                ]
                `shouldBe` Right
                    SmokeConfig
                        { expectedLogin = Login "alice"
                        , org = Org "acme"
                        , team = TeamSlug "core"
                        }

        it "rejects a missing --expected-login" $ do
            parseSmokeArgs ["--org", "acme"]
                `shouldSatisfy` either
                    (T.isInfixOf "--expected-login")
                    (const False)

    describe "renderLoginCheck" $ do
        it "confirms a matching login" $ do
            renderLoginCheck (Login "alice") (Right (Login "alice"))
                `shouldSatisfy` either (const False) (T.isInfixOf "alice")

        it "reports a login mismatch with both logins" $ do
            let outcome =
                    renderLoginCheck
                        (Login "alice")
                        (Right (Login "bob"))
            outcome `shouldSatisfy` isLeft
            outcome `shouldSatisfy` leftSatisfies (T.isInfixOf "alice")
            outcome `shouldSatisfy` leftSatisfies (T.isInfixOf "bob")

        it "reports a whoami error with its status" $ do
            renderLoginCheck
                (Login "alice")
                (Left (GitHubError 401 "bad creds"))
                `shouldSatisfy` leftSatisfies (T.isInfixOf "401")

    describe "renderMembershipCheck" $ do
        it "succeeds only on Active membership" $ do
            renderMembershipCheck Active
                `shouldSatisfy` either (const False) (const True)

        it "fails on NotMember" $ do
            renderMembershipCheck NotMember `shouldSatisfy` isLeft

        it "fails on TokenInvalid" $ do
            renderMembershipCheck TokenInvalid `shouldSatisfy` isLeft

        it "fails on Pending" $ do
            renderMembershipCheck Pending `shouldSatisfy` isLeft

        it "fails on SSORequired and surfaces the URL" $ do
            renderMembershipCheck (SSORequired "https://sso.example")
                `shouldSatisfy` leftSatisfies
                    (T.isInfixOf "https://sso.example")

        it "fails on OtherError and surfaces the status" $ do
            renderMembershipCheck (OtherError 500 "boom")
                `shouldSatisfy` leftSatisfies (T.isInfixOf "500")

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

leftSatisfies :: (a -> Bool) -> Either a b -> Bool
leftSatisfies p = either p (const False)
