{-# LANGUAGE OverloadedStrings #-}

-- | Runtime configuration for the Antithesis proxy daemon.
module Proxy.Antithesis.Config
    ( Settings (..)
    , loadSettings
    , loadSettingsWith
    )
where

import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lib.GitHub.Auth.TeamCheck (Org (..), TeamSlug (..))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Settings = Settings
    { settingsBindAddr :: String
    , settingsBindPort :: Int
    , settingsAntithesisUrl :: String
    , settingsAntithesisUser :: BC.ByteString
    , settingsAntithesisPassword :: BC.ByteString
    , settingsAuthorizedOrg :: Org
    , settingsAuthorizedTeam :: TeamSlug
    , settingsMembershipTtlSeconds :: Int
    , settingsLogLevel :: Text
    }
    deriving stock (Eq, Show)

loadSettings :: IO Settings
loadSettings = loadSettingsWith lookupEnv BC.readFile

loadSettingsWith
    :: (String -> IO (Maybe String))
    -> (FilePath -> IO BC.ByteString)
    -> IO Settings
loadSettingsWith lookupSetting readSecret = do
    passwordFile <-
        settingString
            lookupSetting
            "MOOG_ANTITHESIS_PASSWORD_FILE"
            "/run/secrets/antithesis-password"
    password <- stripTrailingNewline <$> readSecret passwordFile
    Settings
        <$> settingString lookupSetting "MOOG_PROXY_BIND_ADDR" "0.0.0.0"
        <*> settingInt lookupSetting "MOOG_PROXY_BIND_PORT" 8080
        <*> settingString
            lookupSetting
            "MOOG_ANTITHESIS_URL"
            "https://amaru-cardano.antithesis.com"
        <*> ( BC.pack
                <$> settingString
                    lookupSetting
                    "MOOG_ANTITHESIS_USER"
                    "pragma"
            )
        <*> pure password
        <*> ( Org . T.pack
                <$> settingString
                    lookupSetting
                    "MOOG_PROXY_AUTHORIZED_ORG"
                    "pragma-org"
            )
        <*> ( TeamSlug . T.pack
                <$> settingString
                    lookupSetting
                    "MOOG_PROXY_AUTHORIZED_TEAM"
                    "antithesis-access"
            )
        <*> settingInt lookupSetting "MOOG_PROXY_MEMBERSHIP_TTL_SEC" 60
        <*> ( T.pack
                <$> settingString lookupSetting "MOOG_PROXY_LOG_LEVEL" "info"
            )

settingString
    :: (String -> IO (Maybe String))
    -> String
    -> String
    -> IO String
settingString lookupSetting name defaultValue =
    fromMaybe defaultValue <$> lookupSetting name

settingInt
    :: (String -> IO (Maybe String))
    -> String
    -> Int
    -> IO Int
settingInt lookupSetting name defaultValue = do
    raw <- lookupSetting name
    pure $ fromMaybe defaultValue $ raw >>= readMaybe

stripTrailingNewline :: BC.ByteString -> BC.ByteString
stripTrailingNewline =
    BC.dropWhileEnd $ \c -> c == '\n' || c == '\r'
