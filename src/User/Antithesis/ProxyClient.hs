{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | HTTP client for the Moog Antithesis proxy.
--
-- The CLI talks to the proxy via two derived servant-client variants
-- (one regular non-streaming for the JSON endpoints, one streaming for
-- the NDJSON endpoints). Both flow through:
--
--   * 'mkProxyEnv' — builds the 'ClientEnv' with the user's GitHub
--     OAuth token injected as @Authorization: Bearer@;
--   * 'runWithRefresh' (JSON) and 'streamWithRefresh' (NDJSON) — handle
--     the acquire-token / retry-on-401 / classify-error loop.
module User.Antithesis.ProxyClient
    ( ClientErr (..)
    , ProxyUrl (..)
    , defaultProxyUrl
    , proxyUrlFromEnv
    , mkProxyEnv
    , runWithRefresh
    , streamWithRefresh
    , runsRequestWithRefresh
    )
where

import Control.Exception (SomeException, try)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAuthorization, statusCode)
import Proxy.Antithesis.Api
    ( JsonClient (..)
    , antithesisProxyJsonClient
    )
import Servant.API (SourceIO)
import Servant.Client
    ( ClientEnv
    , ClientError (..)
    , ClientM
    , defaultMakeClientRequest
    , makeClientRequest
    , mkClientEnv
    , parseBaseUrl
    , responseBody
    , responseHeaders
    , responseStatusCode
    , runClientM
    )
import Servant.Client.Streaming qualified as Stream
import Servant.Types.SourceT (foreach)
import System.Environment (lookupEnv)
import System.IO (Handle, stderr)

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

-- | Construct a 'ClientEnv' that points at the Moog proxy and injects
-- the supplied GitHub OAuth token as @Authorization: Bearer@.
mkProxyEnv :: ProxyUrl -> OAuthToken -> IO ClientEnv
mkProxyEnv (ProxyUrl url) token = do
    manager <- newManager tlsManagerSettings
    baseUrl <- parseBaseUrl url
    pure $ withBearer token $ mkClientEnv manager baseUrl

withBearer :: OAuthToken -> ClientEnv -> ClientEnv
withBearer token env =
    let injecting baseUrl req = do
            baseRequest <- defaultMakeClientRequest baseUrl req
            let bearer = (hAuthorization, "Bearer " <> unOAuthToken token)
                headers = bearer : filter ((/= hAuthorization) . fst) (HC.requestHeaders baseRequest)
            pure $ baseRequest {HC.requestHeaders = headers}
     in env {makeClientRequest = injecting}

-- | JSON runner: acquire a token, run a regular non-streaming
-- 'ClientM' action against the proxy, and retry once after invalidating
-- the cached token on a 401.
runWithRefresh
    :: ProxyUrl
    -> IO OAuthToken
    -> IO ()
    -> ClientM a
    -> IO (Either ClientErr a)
runWithRefresh proxyUrl acquireToken invalidateToken action = do
    token <- acquireToken
    first <- runOnce proxyUrl token action
    case first of
        Left Unauthorized -> do
            invalidateToken
            refreshed <- acquireToken
            runOnce proxyUrl refreshed action
        _ -> pure first

runOnce
    :: ProxyUrl
    -> OAuthToken
    -> ClientM a
    -> IO (Either ClientErr a)
runOnce proxyUrl token action = do
    envResult <- try @SomeException (mkProxyEnv proxyUrl token)
    case envResult of
        Left e ->
            pure $ Left $ ProxyFailure $ T.pack $ show e
        Right env -> do
            requestResult <- try @SomeException (runClientM action env)
            pure $ case requestResult of
                Left e ->
                    Left $ ProxyFailure $ T.pack $ show e
                Right (Left err) ->
                    Left $ fromClientError err
                Right (Right v) ->
                    Right v

-- | Streaming runner: acquire a token, open a streaming request to the
-- proxy, and drain each chunk of the resulting 'SourceIO ByteString'
-- through the @consume@ callback inside a 'withClientM' bracket so the
-- upstream connection stays alive while bytes flow. On a 401, the
-- cached token is invalidated and the request is retried once with a
-- fresh token.
streamWithRefresh
    :: ProxyUrl
    -> IO OAuthToken
    -> IO ()
    -> Stream.ClientM (SourceIO ByteString)
    -> (ByteString -> IO ())
    -> IO (Either ClientErr ())
streamWithRefresh proxyUrl acquireToken invalidateToken action consume = do
    token <- acquireToken
    first <- streamOnce proxyUrl token action consume
    case first of
        Left Unauthorized -> do
            invalidateToken
            refreshed <- acquireToken
            streamOnce proxyUrl refreshed action consume
        _ -> pure first

streamOnce
    :: ProxyUrl
    -> OAuthToken
    -> Stream.ClientM (SourceIO ByteString)
    -> (ByteString -> IO ())
    -> IO (Either ClientErr ())
streamOnce proxyUrl token action consume = do
    envResult <- try @SomeException (mkProxyEnv proxyUrl token)
    case envResult of
        Left e ->
            pure $ Left $ ProxyFailure $ T.pack $ show e
        Right env -> do
            outcomeRef <- newIORef (Right ())
            result <-
                try @SomeException $
                    Stream.withClientM action env $ \case
                        Left err -> do
                            modifyIORef' outcomeRef (const (Left (fromClientError err)))
                        Right source ->
                            foreach
                                (\msg -> do
                                    modifyIORef' outcomeRef (const (Left (ProxyFailure (T.pack msg))))
                                )
                                consume
                                source
            case result of
                Left e ->
                    pure $ Left $ ProxyFailure $ T.pack $ show e
                Right () ->
                    readIORef outcomeRef

fromClientError :: ClientError -> ClientErr
fromClientError = \case
    FailureResponse _ resp ->
        let code = statusCode (responseStatusCode resp)
            body = responseBody resp
            ssoUrl = lookup "X-Moog-SSO-Url" (toList (responseHeaders resp))
         in case code of
                401 -> Unauthorized
                403 -> Forbidden body ssoUrl
                _ ->
                    ProxyFailure $
                        "proxy request failed with status " <> T.pack (show code)
    DecodeFailure msg _ -> InvalidJson msg
    UnsupportedContentType _ _ -> InvalidJson "unsupported content type from proxy"
    InvalidContentTypeHeader _ -> InvalidJson "invalid content type header from proxy"
    ConnectionError e -> ProxyFailure $ "proxy connection failed: " <> T.pack (show e)

-- | Backwards-compatible shim used by the existing @runs@ subcommand.
runsRequestWithRefresh
    :: ProxyUrl
    -> IO OAuthToken
    -> IO ()
    -> IO (Either ClientErr Value)
runsRequestWithRefresh proxyUrl acquireToken invalidateToken =
    runWithRefresh proxyUrl acquireToken invalidateToken $
        listRuns antithesisProxyJsonClient Nothing Nothing

-- Local declaration to keep imports tidy.
_unusedHandle :: Handle
_unusedHandle = stderr

_unusedBs :: ByteString
_unusedBs = BS.empty
