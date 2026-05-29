{-# LANGUAGE OverloadedStrings #-}

module Proxy.Antithesis.ConfigSpec
    ( spec
    )
where

import Data.ByteString.Char8 qualified as BC
import Lib.GitHub.Auth.TeamCheck (Org (..), TeamSlug (..))
import Proxy.Antithesis.Config
    ( Settings (..)
    , loadSettingsWith
    )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "Proxy.Antithesis.Config" $ do
        it "loads documented defaults and reads the API key file" $ do
            settings <-
                loadSettingsWith
                    (lookupEnvFrom [])
                    (readFileFrom [("/run/secrets/antithesis-api-key", "secret\n")])

            settingsBindAddr settings `shouldBe` "0.0.0.0"
            settingsBindPort settings `shouldBe` 8080
            settingsAntithesisUrl settings
                `shouldBe` "https://amaru-cardano.antithesis.com"
            settingsAntithesisApiKey settings `shouldBe` "secret"
            settingsAuthorizedOrg settings `shouldBe` Org "pragma-org"
            settingsAuthorizedTeam settings `shouldBe` TeamSlug "antithesis-access"
            settingsMembershipTtlSeconds settings `shouldBe` 60
            settingsLogLevel settings `shouldBe` "info"

        it "honours environment overrides" $ do
            settings <-
                loadSettingsWith
                    ( lookupEnvFrom
                        [ ("MOOG_PROXY_BIND_ADDR", "127.0.0.1")
                        , ("MOOG_PROXY_BIND_PORT", "19090")
                        , ("MOOG_ANTITHESIS_URL", "http://upstream")
                        , ("MOOG_ANTITHESIS_API_KEY_FILE", "/tmp/key")
                        , ("MOOG_PROXY_AUTHORIZED_ORG", "my-org")
                        , ("MOOG_PROXY_AUTHORIZED_TEAM", "my-team")
                        , ("MOOG_PROXY_MEMBERSHIP_TTL_SEC", "5")
                        , ("MOOG_PROXY_LOG_LEVEL", "debug")
                        ]
                    )
                    (readFileFrom [("/tmp/key", "top-secret\n")])

            settingsBindAddr settings `shouldBe` "127.0.0.1"
            settingsBindPort settings `shouldBe` 19090
            settingsAntithesisUrl settings `shouldBe` "http://upstream"
            settingsAntithesisApiKey settings `shouldBe` "top-secret"
            settingsAuthorizedOrg settings `shouldBe` Org "my-org"
            settingsAuthorizedTeam settings `shouldBe` TeamSlug "my-team"
            settingsMembershipTtlSeconds settings `shouldBe` 5
            settingsLogLevel settings `shouldBe` "debug"

lookupEnvFrom :: [(String, String)] -> String -> IO (Maybe String)
lookupEnvFrom env name = pure $ lookup name env

readFileFrom :: [(FilePath, BC.ByteString)] -> FilePath -> IO BC.ByteString
readFileFrom files path =
    case lookup path files of
        Just contents -> pure contents
        Nothing -> fail $ "unexpected secret file: " <> path
