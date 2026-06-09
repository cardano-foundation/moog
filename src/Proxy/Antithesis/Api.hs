{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | One source of truth for the Antithesis upstream API surface that
-- @moog-antithesis-proxy@ exposes and that the CLI calls back through.
--
-- The full proxy API is the concatenation of two sub-APIs:
--
--   * 'AntithesisProxyJsonAPI' — runs list, run details, properties.
--     All have type @Get '[PlainJSON, JSON] Value@. Both the proxy
--     server and the CLI use the regular non-streaming
--     'Servant.Client.ClientM' to talk to upstream.
--
--   * 'AntithesisProxyStreamAPI' — events search, logs at a moment,
--     build logs. All have type @StreamGet NoFraming PlainStream
--     (SourceIO ByteString)@. The CLI drains the source to stdout
--     inside a 'Servant.Client.Streaming.withClientM' callback so the
--     upstream connection stays open during the stream. The proxy
--     server forwards the upstream byte stream directly via
--     "Proxy.Antithesis.Server" using @http-client@ to keep the
--     upstream connection alive across the WAI streaming-body emission.
--
-- The split is necessary because @servant-client@ and
-- @servant-client-streaming@ ship two distinct 'ClientM' monads; an API
-- with both 'Get' and 'StreamGet' cannot be derived under a single one.
module Proxy.Antithesis.Api
    ( AntithesisProxyAPI
    , antithesisProxyAPI
    , AntithesisProxyJsonAPI
    , antithesisProxyJsonAPI
    , AntithesisProxyStreamAPI
    , antithesisProxyStreamAPI
    , PlainJSON
    , PlainStream
    , JsonClient (..)
    , antithesisProxyJsonClient
    , StreamClient (..)
    , antithesisProxyStreamClient
    )
where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.List.NonEmpty qualified as NE
import Network.HTTP.Media ((//))
import Servant.API
    ( Accept (..)
    , Capture
    , Get
    , MimeRender (..)
    , MimeUnrender (..)
    , NoFraming
    , QueryParam
    , SourceIO
    , StreamGet
    , (:<|>) (..)
    , (:>)
    )
import Servant.Client (ClientM, client)
import Servant.Client.Streaming qualified as Stream

-- | MIME type for the upstream JSON endpoints. The Antithesis tenant
-- advertises @text/plain@; well-behaved upstream proxies might also
-- send @application/json@. Accept both so the @servant-client@
-- response decoder doesn't reject either.
data PlainJSON

instance Accept PlainJSON where
    contentType _ = "text" // "plain"
    contentTypes _ = NE.fromList ["text" // "plain", "application" // "json"]

instance MimeRender PlainJSON Value where
    mimeRender _ = Aeson.encode

instance MimeUnrender PlainJSON Value where
    mimeUnrender _ = Aeson.eitherDecode

-- | MIME type used by the NDJSON streaming endpoints — bytes flow
-- through untouched.
data PlainStream

instance Accept PlainStream where
    contentType _ = "text" // "plain"

instance MimeRender PlainStream ByteString where
    mimeRender _ = LBS.fromStrict

instance MimeUnrender PlainStream ByteString where
    mimeUnrender _ = Right . LBS.toStrict

-- | JSON sub-API. All handlers return decoded 'Aeson.Value's.
type AntithesisProxyJsonAPI =
    "api"
        :> "v0"
        :> "openapi.json"
        :> Get '[PlainJSON] Value
        :<|> "api"
            :> "v0"
            :> "runs"
            :> QueryParam "limit" Int
            -- Antithesis paginates with @after=@ (a created-before token),
            -- NOT @cursor=@. Sending the wrong name makes the upstream
            -- silently ignore it and re-serve page 1 with an unchanged
            -- @next_cursor@, which loops 'listAllRuns' forever (OOM).
            :> QueryParam "after" Text
            :> Get '[PlainJSON] Value
        :<|> "api"
            :> "v0"
            :> "runs"
            :> Capture "run_id" Text
            :> Get '[PlainJSON] Value
        :<|> "api"
            :> "v0"
            :> "runs"
            :> Capture "run_id" Text
            :> "properties"
            :> Get '[PlainJSON] Value

-- | Streaming sub-API. All three handlers return a 'SourceIO ByteString'
-- and the server emits chunks verbatim (NoFraming).
type AntithesisProxyStreamAPI =
    "api"
        :> "v0"
        :> "runs"
        :> Capture "run_id" Text
        :> "events"
        :> QueryParam "q" Text
        :> StreamGet NoFraming PlainStream (SourceIO ByteString)
        :<|> "api"
            :> "v0"
            :> "runs"
            :> Capture "run_id" Text
            :> "logs"
            :> QueryParam "input_hash" Text
            :> QueryParam "vtime" Text
            :> StreamGet NoFraming PlainStream (SourceIO ByteString)
        :<|> "api"
            :> "v0"
            :> "runs"
            :> Capture "run_id" Text
            :> "build_logs"
            :> StreamGet NoFraming PlainStream (SourceIO ByteString)

-- | Combined API exposed by the proxy server.
type AntithesisProxyAPI = AntithesisProxyJsonAPI :<|> AntithesisProxyStreamAPI

antithesisProxyAPI :: Proxy AntithesisProxyAPI
antithesisProxyAPI = Proxy

antithesisProxyJsonAPI :: Proxy AntithesisProxyJsonAPI
antithesisProxyJsonAPI = Proxy

antithesisProxyStreamAPI :: Proxy AntithesisProxyStreamAPI
antithesisProxyStreamAPI = Proxy

-- | Auto-derived JSON-endpoint client (regular non-streaming
-- 'Servant.Client.ClientM').
data JsonClient = JsonClient
    { getOpenApi :: ClientM Value
    , listRuns :: Maybe Int -> Maybe Text -> ClientM Value
    , getRun :: Text -> ClientM Value
    , getProperties :: Text -> ClientM Value
    }

antithesisProxyJsonClient :: JsonClient
antithesisProxyJsonClient =
    let getOpenApi_ :<|> listRuns_ :<|> getRun_ :<|> getProperties_ =
            client antithesisProxyJsonAPI
     in JsonClient
            { getOpenApi = getOpenApi_
            , listRuns = listRuns_
            , getRun = getRun_
            , getProperties = getProperties_
            }

-- | Auto-derived streaming client
-- ('Servant.Client.Streaming.ClientM'). Consumers MUST use
-- 'Servant.Client.Streaming.withClientM' so the upstream connection
-- stays open while the 'SourceIO' is drained.
data StreamClient = StreamClient
    { getEvents :: Text -> Maybe Text -> Stream.ClientM (SourceIO ByteString)
    , getLogs :: Text -> Maybe Text -> Maybe Text -> Stream.ClientM (SourceIO ByteString)
    , getBuildLogs :: Text -> Stream.ClientM (SourceIO ByteString)
    }

antithesisProxyStreamClient :: StreamClient
antithesisProxyStreamClient =
    let getEvents_ :<|> getLogs_ :<|> getBuildLogs_ =
            Stream.client antithesisProxyStreamAPI
     in StreamClient
            { getEvents = getEvents_
            , getLogs = getLogs_
            , getBuildLogs = getBuildLogs_
            }
