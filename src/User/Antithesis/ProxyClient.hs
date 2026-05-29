{-# LANGUAGE OverloadedStrings #-}

-- | HTTP client for the Moog Antithesis proxy.
module User.Antithesis.ProxyClient
    ( ClientErr (..)
    , ProxyUrl (..)
    , defaultProxyUrl
    , proxyUrlFromEnv
    , runsRequest
    , runsRequestWithRefresh
    )
where

import Control.Exception (try)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Network.HTTP.Client
    ( HttpException
    , Request (method, requestHeaders)
    , Response (responseBody, responseHeaders, responseStatus)
    , httpLbs
    , newManager
    , parseRequest
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types
    ( hAccept
    , hAuthorization
    , statusCode
    )
import System.Environment (lookupEnv)

newtype ProxyUrl = ProxyUrl String
    deriving stock (Eq, Show)

data ClientErr
    = Unauthorized
    | Forbidden LBS.ByteString (Maybe ByteString)
    | ProxyFailure Text
    | InvalidJson Text
    deriving stock (Eq, Show)

defaultProxyUrl :: ProxyUrl
defaultProxyUrl = ProxyUrl "https://antithesis-proxy.plutimus.com"

proxyUrlFromEnv :: IO ProxyUrl
proxyUrlFromEnv =
    maybe defaultProxyUrl ProxyUrl <$> lookupEnv "MOOG_ANTITHESIS_PROXY_URL"

runsRequest :: ProxyUrl -> OAuthToken -> IO (Either ClientErr Value)
runsRequest proxyUrl token = do
    manager <- newManager tlsManagerSettings
    requestResult <- try @HttpException $ do
        baseRequest <- parseRequest $ runsUrl proxyUrl
        httpLbs
            baseRequest
                { method = "GET"
                , requestHeaders =
                    [ (hAccept, "application/json")
                    , (hAuthorization, "Bearer " <> unOAuthToken token)
                    ]
                }
            manager
    pure $ case requestResult of
        Left err ->
            Left $ ProxyFailure $ T.pack $ show err
        Right response ->
            classifyResponse response

runsRequestWithRefresh
    :: ProxyUrl
    -> IO OAuthToken
    -> IO ()
    -> IO (Either ClientErr Value)
runsRequestWithRefresh proxyUrl acquireToken invalidateToken = do
    token <- acquireToken
    first <- runsRequest proxyUrl token
    case first of
        Left Unauthorized -> do
            invalidateToken
            refreshedToken <- acquireToken
            runsRequest proxyUrl refreshedToken
        _ -> pure first

classifyResponse :: Response LBS.ByteString -> Either ClientErr Value
classifyResponse response =
    let code = statusCode $ responseStatus response
        body = responseBody response
     in case code of
            200 -> case Aeson.eitherDecode body of
                Left err -> Left $ InvalidJson $ T.pack err
                Right value -> Right value
            401 -> Left Unauthorized
            403 ->
                Left $
                    Forbidden body $
                        lookup "X-Moog-SSO-Url" $
                            responseHeaders response
            _
                | code >= 500 ->
                    Left $
                        ProxyFailure $
                            "proxy request failed with status " <> T.pack (show code)
                | otherwise ->
                    Left $
                        ProxyFailure $
                            "proxy request failed with status " <> T.pack (show code)

runsUrl :: ProxyUrl -> String
runsUrl (ProxyUrl baseUrl) =
    stripTrailingSlash baseUrl <> "/api/v1/runs"

stripTrailingSlash :: String -> String
stripTrailingSlash =
    reverse . dropWhile (== '/') . reverse
