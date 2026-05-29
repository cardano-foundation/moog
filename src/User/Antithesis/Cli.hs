{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | User-facing Antithesis CLI commands.
--
-- The JSON subcommands ('Runs', 'Run', 'Properties') return decoded
-- 'Aeson.Value's via the regular 'Servant.Client.ClientM' transport;
-- the renderer in @Main@ prints them as canonical JSON.
--
-- The streaming subcommands ('Events', 'Logs', 'BuildLogs') drain a
-- 'Servant.Client.Streaming.ClientM' (SourceIO ByteString) directly to
-- @stdout@ inside a 'withClientM' bracket, then exit with code 0 (or a
-- non-zero auth/proxy failure code). They produce no JSON output —
-- their output is the upstream NDJSON byte stream verbatim.
module User.Antithesis.Cli
    ( AntithesisCommand (..)
    , antithesisCmd
    , antithesisCommandParser
    )
where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text (Text)
import Lib.Box (Box (..))
import Lib.JSON.Canonical.Extra ()
import OptEnvConf
    ( Parser
    , auto
    , command
    , commands
    , help
    , long
    , metavar
    , option
    , optional
    , reader
    , setting
    , str
    )
import Proxy.Antithesis.Api
    ( JsonClient (..)
    , StreamClient (..)
    , antithesisProxyJsonClient
    , antithesisProxyStreamClient
    )
import Servant.Client qualified as JsonClient (ClientM)
import Servant.Client.Streaming qualified as Stream
import Servant.API (SourceIO)
import Data.ByteString (ByteString)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr, stdout)
import User.Antithesis.Login
    ( defaultTokenFile
    , ensureToken
    , evictCachedToken
    )
import User.Antithesis.ProxyClient
    ( ClientErr (..)
    , proxyUrlFromEnv
    , runWithRefresh
    , streamWithRefresh
    )

data AntithesisCommand a where
    Runs :: Maybe Int -> Maybe Text -> AntithesisCommand Aeson.Value
    Run :: Text -> AntithesisCommand Aeson.Value
    Properties :: Text -> AntithesisCommand Aeson.Value
    Events :: Text -> Maybe Text -> AntithesisCommand Aeson.Value
    Logs :: Text -> Maybe Text -> Maybe Text -> AntithesisCommand Aeson.Value
    BuildLogs :: Text -> AntithesisCommand Aeson.Value

antithesisCmd :: AntithesisCommand a -> IO a
antithesisCmd = \case
    Runs lim cur ->
        runJson $ listRuns jc lim cur
    Run rid ->
        runJson $ getRun jc rid
    Properties rid ->
        runJson $ getProperties jc rid
    Events rid q ->
        runStream $ getEvents sc rid q
    Logs rid hh vt ->
        runStream $ getLogs sc rid hh vt
    BuildLogs rid ->
        runStream $ getBuildLogs sc rid
  where
    jc = antithesisProxyJsonClient
    sc = antithesisProxyStreamClient

-- | Run a JSON action via the regular 'Servant.Client.ClientM'.
-- Returns the decoded 'Aeson.Value' for the top-level Moog renderer.
runJson :: JsonClient.ClientM Aeson.Value -> IO Aeson.Value
runJson action = do
    proxyUrl <- proxyUrlFromEnv
    result <-
        runWithRefresh
            proxyUrl
            ensureToken
            (defaultTokenFile >>= evictCachedToken)
            action
    case result of
        Right value -> pure value
        Left err -> do
            hPutStrLn stderr $ renderClientErr err
            exitWith $ ExitFailure $ clientErrExitCode err

-- | Drain a streaming action's 'SourceIO ByteString' directly to
-- stdout, byte-for-byte. Returns 'Aeson.Null' so the top-level renderer
-- emits nothing extra (the streamed bytes are the output).
runStream :: Stream.ClientM (SourceIO ByteString) -> IO Aeson.Value
runStream action = do
    proxyUrl <- proxyUrlFromEnv
    result <-
        streamWithRefresh
            proxyUrl
            ensureToken
            (defaultTokenFile >>= evictCachedToken)
            action
            (BS.hPut stdout)
    case result of
        Right () -> pure Aeson.Null
        Left err -> do
            hPutStrLn stderr $ renderClientErr err
            exitWith $ ExitFailure $ clientErrExitCode err

antithesisCommandParser :: Parser (Box AntithesisCommand)
antithesisCommandParser =
    commands
        [ command "runs" runsHelp $
            (\lim cur -> Box (Runs lim cur))
                <$> optional limitOption
                <*> optional cursorOption
        , command "run" runHelp $
            Box . Run <$> runIdOption
        , command "properties" propertiesHelp $
            Box . Properties <$> runIdOption
        , command "events" eventsHelp $
            (\rid q -> Box (Events rid q))
                <$> runIdOption
                <*> optional eventQueryOption
        , command "logs" logsHelp $
            (\rid h v -> Box (Logs rid h v))
                <$> runIdOption
                <*> optional inputHashOption
                <*> optional vtimeOption
        , command "build-logs" buildLogsHelp $
            Box . BuildLogs <$> runIdOption
        ]

runIdOption :: Parser Text
runIdOption =
    setting
        [ help "Antithesis run id (e.g. abc123…-54-7)"
        , metavar "RUN_ID"
        , reader str
        , long "run-id"
        , option
        ]

limitOption :: Parser Int
limitOption =
    setting
        [ help "Page size (max 100; default server-side)"
        , metavar "N"
        , reader auto
        , long "limit"
        , option
        ]

cursorOption :: Parser Text
cursorOption =
    setting
        [ help "Opaque pagination cursor returned by a previous call"
        , metavar "CURSOR"
        , reader str
        , long "cursor"
        , option
        ]

eventQueryOption :: Parser Text
eventQueryOption =
    setting
        [ help "Free-text substring search inside the run's events stream"
        , metavar "QUERY"
        , reader str
        , long "q"
        , option
        ]

inputHashOption :: Parser Text
inputHashOption =
    setting
        [ help "Antithesis input hash identifying a moment in the run"
        , metavar "HASH"
        , reader str
        , long "input-hash"
        , option
        ]

vtimeOption :: Parser Text
vtimeOption =
    setting
        [ help "Antithesis vtime identifying a moment in the run"
        , metavar "VTIME"
        , reader str
        , long "vtime"
        , option
        ]

runsHelp :: String
runsHelp =
    "List Antithesis runs via the Moog proxy. \
    \Pagination via --limit and --cursor. \
    \Exit codes: 0 success, 2 authorization or SSO failure, \
    \3 proxy or network failure, 4 invalid JSON from proxy."

runHelp :: String
runHelp =
    "Read the details of a single Antithesis run, including the \
    \failure moment for incomplete runs."

propertiesHelp :: String
propertiesHelp =
    "Read property pass/fail results for a completed Antithesis run \
    \(404 for runs still in progress)."

eventsHelp :: String
eventsHelp =
    "Stream the run's matching events as newline-delimited JSON \
    \(free-text substring search via --q; capped at 50 results \
    \by upstream). Bytes are written verbatim to stdout."

logsHelp :: String
logsHelp =
    "Stream logs at a specific moment of an Antithesis run as \
    \newline-delimited JSON, identified by --input-hash and --vtime."

buildLogsHelp :: String
buildLogsHelp =
    "Stream the build logs for an Antithesis run as newline-delimited JSON."

clientErrExitCode :: ClientErr -> Int
clientErrExitCode = \case
    Unauthorized -> 2
    Forbidden{} -> 2
    ProxyFailure{} -> 3
    InvalidJson{} -> 4

renderClientErr :: ClientErr -> String
renderClientErr = \case
    Unauthorized ->
        "proxy rejected refreshed GitHub token with 401"
    Forbidden body maybeSsoUrl ->
        "proxy authorization failed: "
            <> LBS8.unpack body
            <> maybe "" (("\nSSO URL: " <>) . show) maybeSsoUrl
    ProxyFailure message ->
        "proxy request failed: " <> show message
    InvalidJson message ->
        "proxy returned non-JSON response: " <> show message
