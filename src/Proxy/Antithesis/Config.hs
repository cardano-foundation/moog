{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Runtime configuration for the Antithesis proxy daemon.
module Proxy.Antithesis.Config
    ( Settings (..)
    , BrowserSettings (..)
    , loadSettings
    , loadSettingsWith
    )
where

import Control.Exception (IOException, try)
import Data.ByteString.Char8 qualified as BC
import Data.Char (isAsciiLower, isDigit)
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
    , settingsAntithesisApiKey :: BC.ByteString
    , settingsAuthorizedOrg :: Org
    , settingsAuthorizedTeam :: TeamSlug
    , settingsMembershipTtlSeconds :: Int
    , settingsLogLevel :: Text
    , settingsBrowser :: Maybe BrowserSettings
    }
    deriving stock (Eq, Show)

-- | Explicit server-side browser OAuth settings. Display omits credentials.
data BrowserSettings = BrowserSettings
    { browserClientId :: BC.ByteString
    , browserClientSecret :: BC.ByteString
    , browserCallback :: BC.ByteString
    , browserSessionSeconds :: Int
    , browserStateSeconds :: Int
    }
    deriving stock (Eq)

instance Show BrowserSettings where
    show _ = "BrowserSettings <redacted>"

loadSettings :: IO Settings
loadSettings = loadSettingsWith lookupEnv BC.readFile

loadSettingsWith
    :: (String -> IO (Maybe String))
    -> (FilePath -> IO BC.ByteString)
    -> IO Settings
loadSettingsWith lookupSetting readSecret = do
    apiKeyFile <-
        settingString
            lookupSetting
            "MOOG_ANTITHESIS_API_KEY_FILE"
            "/run/secrets/antithesis-api-key"
    apiKey <- stripTrailingNewline <$> readSecret apiKeyFile
    browser <- loadBrowserSettings lookupSetting readSecret
    Settings
        <$> settingString lookupSetting "MOOG_PROXY_BIND_ADDR" "0.0.0.0"
        <*> settingInt lookupSetting "MOOG_PROXY_BIND_PORT" 8080
        <*> settingString
            lookupSetting
            "MOOG_ANTITHESIS_URL"
            "https://amaru-cardano.antithesis.com"
        <*> pure apiKey
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
        <*> pure browser

-- Partial, invalid or unreadable browser installation disables only the
-- browser subtree. Existing bearer API operation still needs its API key.
loadBrowserSettings
    :: (String -> IO (Maybe String))
    -> (FilePath -> IO BC.ByteString)
    -> IO (Maybe BrowserSettings)
loadBrowserSettings lookupSetting readSecret = do
    client <- lookupSetting "MOOG_PROXY_BROWSER_CLIENT_ID"
    secretFile <- lookupSetting "MOOG_PROXY_BROWSER_CLIENT_SECRET_FILE"
    callback <- lookupSetting "MOOG_PROXY_BROWSER_CALLBACK_URL"
    sessionRaw <- lookupSetting "MOOG_PROXY_BROWSER_SESSION_TTL_SEC"
    stateRaw <- lookupSetting "MOOG_PROXY_BROWSER_STATE_TTL_SEC"
    case ( client
         , secretFile
         , callback
         , positive 3600 sessionRaw
         , positive 300 stateRaw
         ) of
        ( Just clientId
            , Just file
            , Just callbackUrl
            , Just sessionTtl
            , Just stateTtl
            )
                | not (null clientId)
                , not (null file)
                , validCallback callbackUrl -> do
                    secret <- try @IOException $ stripTrailingNewline <$> readSecret file
                    pure $ case secret of
                        Right value
                            | not (BC.null value) ->
                                Just
                                    $ BrowserSettings
                                        (BC.pack clientId)
                                        value
                                        (BC.pack callbackUrl)
                                        sessionTtl
                                        stateTtl
                        _ -> Nothing
        _ -> pure Nothing
  where
    positive fallback Nothing = Just fallback
    positive _ (Just raw) = do
        value <- readMaybe raw
        if value > 0 && value <= 86400 then Just value else Nothing
    validCallback raw = case T.stripPrefix "https://" (T.pack raw) of
        Just rest -> case T.stripSuffix "/browser/callback" rest of
            Just host -> not (T.null host) && T.all validHost host
            _ -> False
        _ -> False
    validHost c =
        isAsciiLower c
            || isDigit c
            || c `elem` ['.', '-']

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
