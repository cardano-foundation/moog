{-# LANGUAGE OverloadedStrings #-}

module Moog.GitHub.DeviceFlowSmoke
    ( renderTokenEvidence
    , main
    )
where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Lib.GitHub.Auth.DeviceFlow
    ( ClientId (..)
    , DeviceCodeResponse (..)
    , DeviceFlowError
    , OAuthToken (..)
    , runDeviceFlow
    )
import System.Environment (getArgs)
import System.Exit (die)

renderTokenEvidence :: OAuthToken -> Text
renderTokenEvidence (OAuthToken token) =
    T.decodeUtf8 (BS.take 4 token)
        <> " token="
        <> T.pack (show $ BS.length token)

main :: IO ()
main = do
    clientId <- parseClientId =<< getArgs
    result <-
        runDeviceFlow clientId [] $ \DeviceCodeResponse{..} -> do
            putStrLn $ "Verification URI: " <> T.unpack dcVerificationUri
            putStrLn $ "User code: " <> T.unpack dcUserCode
    case result of
        Left err ->
            die $ "GitHub device flow failed: " <> showDeviceFlowError err
        Right token ->
            putStrLn $ "Token evidence: " <> T.unpack (renderTokenEvidence token)

parseClientId :: [String] -> IO ClientId
parseClientId ["--client-id", clientId]
    | not (null clientId) = pure $ ClientId $ T.pack clientId
parseClientId _ =
    die "Usage: moog-github-device-flow-smoke --client-id <id>"

showDeviceFlowError :: DeviceFlowError -> String
showDeviceFlowError = show
