{-# LANGUAGE OverloadedStrings #-}

-- | GitHub OAuth token cache and device-flow login boundary.
module User.Antithesis.Login
    ( defaultTokenFile
    , ensureToken
    , ensureTokenAt
    , evictCachedToken
    , readCachedToken
    , writeCachedToken
    )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lib.GitHub.Auth.DeviceFlow
    ( DeviceCodeResponse (..)
    , DeviceFlowError
    , OAuthToken (..)
    , runDeviceFlow
    )
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , getHomeDirectory
    , removeFile
    )
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files
    ( ownerReadMode
    , ownerWriteMode
    , setFileMode
    , unionFileModes
    )
import System.Posix.Types (FileMode)
import User.Antithesis.Constants
    ( moogOAuthClientId
    , moogOAuthScopes
    )

newtype CachedOAuthToken = CachedOAuthToken OAuthToken
    deriving stock (Eq, Show)

instance Aeson.ToJSON CachedOAuthToken where
    toJSON (CachedOAuthToken (OAuthToken token)) =
        Aeson.object ["access_token" Aeson..= TE.decodeUtf8 token]

instance Aeson.FromJSON CachedOAuthToken where
    parseJSON =
        Aeson.withObject "CachedOAuthToken" $ \objectValue ->
            CachedOAuthToken . OAuthToken . TE.encodeUtf8
                <$> objectValue Aeson..: "access_token"

defaultTokenFile :: IO FilePath
defaultTokenFile = do
    home <- getHomeDirectory
    pure $ home </> ".config" </> "moog" </> "github-oauth.json"

ensureToken :: IO OAuthToken
ensureToken = do
    path <- defaultTokenFile
    ensureTokenAt path $
        runDeviceFlow moogOAuthClientId moogOAuthScopes printDeviceCode

ensureTokenAt
    :: FilePath
    -> IO (Either DeviceFlowError OAuthToken)
    -> IO OAuthToken
ensureTokenAt path acquireToken = do
    cached <- readCachedToken path
    case cached of
        Just token -> pure token
        Nothing -> do
            result <- acquireToken
            case result of
                Left err ->
                    ioError
                        $ userError
                        $ "GitHub device-flow login failed: " <> show err
                Right token -> do
                    writeCachedToken path token
                    pure token

readCachedToken :: FilePath -> IO (Maybe OAuthToken)
readCachedToken path = do
    exists <- doesFileExist path
    if exists
        then do
            body <- LBS.readFile path
            case Aeson.eitherDecode body of
                Left err ->
                    ioError
                        $ userError
                        $ "Failed to parse GitHub OAuth cache: " <> err
                Right (CachedOAuthToken token) -> pure $ Just token
        else pure Nothing

writeCachedToken :: FilePath -> OAuthToken -> IO ()
writeCachedToken path token = do
    createDirectoryIfMissing True $ takeDirectory path
    LBS.writeFile path $ Aeson.encode $ CachedOAuthToken token
    setFileMode path tokenFileMode

evictCachedToken :: FilePath -> IO ()
evictCachedToken path = do
    exists <- doesFileExist path
    if exists
        then removeFile path
        else pure ()

printDeviceCode :: DeviceCodeResponse -> IO ()
printDeviceCode deviceCode =
    hPutStrLn stderr $
        "Visit "
            <> T.unpack (dcVerificationUri deviceCode)
            <> " and enter code "
            <> T.unpack (dcUserCode deviceCode)

tokenFileMode :: FileMode
tokenFileMode = ownerReadMode `unionFileModes` ownerWriteMode
