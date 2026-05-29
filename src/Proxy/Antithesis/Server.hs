{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Server-side implementation of 'AntithesisProxyAPI'.
--
-- The JSON sub-API is implemented by hoisting the auto-derived
-- 'JsonClient' into the Servant 'Handler' monad — every JSON handler is
-- a one-line @runUpstream env (clientFn args…)@.
--
-- The streaming sub-API can't use the same trick because
-- @servant-client@ and @servant-client-streaming@ ship distinct
-- 'ClientM' monads. Instead, each streaming handler opens the upstream
-- response with @http-client@ directly and wraps the response's body
-- reader in a 'SourceIO ByteString' that the WAI server drains. The
-- upstream connection stays open for the full duration of the response
-- emission, and is returned to the pool on EOF.
module Proxy.Antithesis.Server
    ( UpstreamConfig (..)
    , makeUpstreamEnv
    , makeServantApp
    , runUpstream
    , mapClientError
    , bodyReaderToSourceIO
    , forwardUpstreamStream
    )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types (hAuthorization, methodGet, statusCode, statusMessage)
import Proxy.Antithesis.Api
    ( AntithesisProxyAPI
    , AntithesisProxyJsonAPI
    , AntithesisProxyStreamAPI
    , JsonClient (..)
    , antithesisProxyAPI
    , antithesisProxyJsonAPI
    , antithesisProxyJsonClient
    )
import Servant
    ( Application
    , Handler
    , Server
    , ServerError (..)
    , err502
    , err504
    , hoistServer
    , serve
    , throwError
    , (:<|>) (..)
    )
import Servant.API (SourceIO)
import Servant.Client
    ( BaseUrl (..)
    , ClientEnv
    , ClientError (..)
    , ClientM
    , Scheme (..)
    , defaultMakeClientRequest
    , makeClientRequest
    , mkClientEnv
    , responseBody
    , responseStatusCode
    , runClientM
    )
import Servant.Types.SourceT (SourceT (..), StepT (..))

-- | Static upstream configuration: base URL, API key, and a shared
-- http-client manager (used by both the JSON sub-API forwarder and the
-- streaming sub-API forwarder).
data UpstreamConfig = UpstreamConfig
    { upstreamBaseUrl :: BaseUrl
    , upstreamApiKey :: ByteString
    , upstreamManager :: Manager
    }

-- | Build a 'ClientEnv' that injects @Authorization: Bearer <api-key>@
-- on every outgoing request, used for the JSON sub-API.
makeUpstreamEnv :: UpstreamConfig -> ClientEnv
makeUpstreamEnv cfg =
    let env = mkClientEnv (upstreamManager cfg) (upstreamBaseUrl cfg)
        key = upstreamApiKey cfg
        injecting baseUrl req = do
            baseRequest <- defaultMakeClientRequest baseUrl req
            let bearer = (hAuthorization, "Bearer " <> key)
                headers = bearer : filter ((/= hAuthorization) . fst) (HC.requestHeaders baseRequest)
            pure $ baseRequest {HC.requestHeaders = headers}
     in env {makeClientRequest = injecting}

-- | Servant 'Application' for the proxy.
makeServantApp :: UpstreamConfig -> Application
makeServantApp cfg = serve antithesisProxyAPI (server cfg)

server :: UpstreamConfig -> Server AntithesisProxyAPI
server cfg = jsonServer cfg :<|> streamServer cfg

-- The JSON sub-API: hoist the auto-derived JSON client into Handler.
jsonServer :: UpstreamConfig -> Server AntithesisProxyJsonAPI
jsonServer cfg =
    hoistServer
        antithesisProxyJsonAPI
        (runUpstream (makeUpstreamEnv cfg))
        jsonHandlers
  where
    c = antithesisProxyJsonClient
    jsonHandlers =
        getOpenApi c
            :<|> listRuns c
            :<|> getRun c
            :<|> getProperties c

-- The streaming sub-API: each handler opens an upstream streaming
-- response and wraps its body reader in a 'SourceIO ByteString'.
streamServer :: UpstreamConfig -> Server AntithesisProxyStreamAPI
streamServer cfg =
    eventsHandler
        :<|> logsHandler
        :<|> buildLogsHandler
  where
    eventsHandler rid q =
        forward
            ["api", "v0", "runs", rid, "events"]
            [("q", q)]
    logsHandler rid h v =
        forward
            ["api", "v0", "runs", rid, "logs"]
            [("input_hash", h), ("vtime", v)]
    buildLogsHandler rid =
        forward
            ["api", "v0", "runs", rid, "build_logs"]
            []
    forward pathSegments query =
        liftIO $ forwardUpstreamStream cfg pathSegments query

-- | Open a streaming GET on the upstream and return a 'SourceIO
-- ByteString' that yields raw body chunks until the upstream closes,
-- then returns the connection to the pool.
forwardUpstreamStream
    :: UpstreamConfig
    -> [Text]
    -- ^ path segments (no leading slash)
    -> [(BS.ByteString, Maybe Text)]
    -- ^ query parameters; @Nothing@ is dropped
    -> IO (SourceIO ByteString)
forwardUpstreamStream cfg pathSegments query = do
    let base = upstreamBaseUrl cfg
        scheme = case baseUrlScheme base of
            Http -> "http://"
            Https -> "https://"
        host = scheme <> baseUrlHost base
        port =
            case (baseUrlScheme base, baseUrlPort base) of
                (Http, 80) -> ""
                (Https, 443) -> ""
                (_, p) -> ":" <> show p
        path = "/" <> mconcat (zipWith (\i s -> (if i == 0 then "" else "/") <> T.encodeUtf8 s) [0 :: Int ..] pathSegments)
        url = host <> port <> baseUrlPath base <> BC.unpack path
    baseReq <- HC.parseRequest url
    let req =
            baseReq
                { HC.method = methodGet
                , HC.requestHeaders =
                    (hAuthorization, "Bearer " <> upstreamApiKey cfg)
                        : HC.requestHeaders baseReq
                , HC.queryString = renderQuery query
                }
    response <- HC.responseOpen req (upstreamManager cfg)
    pure $ bodyReaderToSourceIO response

-- | Wrap an open http-client streaming response into a 'SourceIO
-- ByteString'. Yields raw chunks until the body reader signals EOF
-- (empty chunk), then closes the response. Skips empty intermediate
-- chunks so callers never see them.
bodyReaderToSourceIO :: HC.Response HC.BodyReader -> SourceIO ByteString
bodyReaderToSourceIO response =
    SourceT $ \k -> k go
  where
    go = Effect $ do
        chunk <- HC.brRead (HC.responseBody response)
        if BS.null chunk
            then do
                HC.responseClose response
                pure Stop
            else pure $ Yield chunk go

-- | Natural transformation for the JSON sub-API: run a 'ClientM' action
-- against the upstream environment and map any client error into a
-- 'ServerError'.
runUpstream :: ClientEnv -> ClientM a -> Handler a
runUpstream env action = do
    result <- liftIO $ try @SomeException (runClientM action env)
    case result of
        Left e ->
            throwError err502 {errBody = LBS.fromStrict $ BC.pack $ "upstream exception: " <> show e}
        Right (Left err) ->
            throwError (mapClientError err)
        Right (Right value) ->
            pure value

-- | Translate a 'ClientError' from the upstream call into a
-- 'ServerError' that the proxy returns to the caller. The status code
-- is preserved for HTTP-layer failures; 5xx bodies are sanitized.
mapClientError :: ClientError -> ServerError
mapClientError = \case
    FailureResponse _ resp ->
        let st = responseStatusCode resp
            code = statusCode st
            body = responseBody resp
            sanitized
                | code >= 500 = "upstream request failed"
                | otherwise = body
         in ServerError
                { errHTTPCode = code
                , errReasonPhrase = BC.unpack (statusMessage st)
                , errBody = sanitized
                , errHeaders = []
                }
    DecodeFailure msg _ ->
        err502 {errBody = "upstream decode failure: " <> LBS.fromStrict (BC.pack (show msg))}
    UnsupportedContentType _ _ ->
        err502 {errBody = "upstream content type unsupported"}
    InvalidContentTypeHeader _ ->
        err502 {errBody = "upstream content type header invalid"}
    ConnectionError _ ->
        err504 {errBody = "upstream connection failed"}

-- | Render a query-parameter list as a URL-encoded query string with
-- a leading @?@. Empty 'Nothing' values are dropped.
renderQuery :: [(BS.ByteString, Maybe Text)] -> BS.ByteString
renderQuery pairs =
    let items =
            [ k <> "=" <> percentEncode (T.encodeUtf8 v)
            | (k, Just v) <- pairs
            ]
     in case items of
            [] -> ""
            xs -> "?" <> BS.intercalate "&" xs

-- | Minimal application/x-www-form-urlencoded percent encoder.
percentEncode :: BS.ByteString -> BS.ByteString
percentEncode = BS.concatMap escapeByte
  where
    escapeByte b
        | isUnreserved b = BS.singleton b
        | otherwise = BS.pack [0x25, hex (b `div` 16), hex (b `mod` 16)]
    isUnreserved b =
        (0x41 <= b && b <= 0x5A) -- A-Z
            || (0x61 <= b && b <= 0x7A) -- a-z
            || (0x30 <= b && b <= 0x39) -- 0-9
            || b `elem` [0x2D, 0x2E, 0x5F, 0x7E] -- - . _ ~
    hex n
        | n < 10 = 0x30 + n
        | otherwise = 0x41 + (n - 10)
