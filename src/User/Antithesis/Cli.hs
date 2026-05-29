-- | User-facing Antithesis CLI commands.
module User.Antithesis.Cli
    ( AntithesisCommand (..)
    , antithesisCmd
    , antithesisCommandParser
    )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Lib.Box (Box (..))
import Lib.JSON.Canonical.Extra ()
import OptEnvConf (Parser, command, commands)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import User.Antithesis.Login
    ( defaultTokenFile
    , ensureToken
    , evictCachedToken
    )
import User.Antithesis.ProxyClient
    ( ClientErr (..)
    , proxyUrlFromEnv
    , runsRequestWithRefresh
    )

data AntithesisCommand a where
    Runs :: AntithesisCommand Aeson.Value

antithesisCmd :: AntithesisCommand a -> IO a
antithesisCmd = \case
    Runs -> runRuns

antithesisCommandParser :: Parser (Box AntithesisCommand)
antithesisCommandParser =
    commands
        [ command "runs" runsHelp $
            pure $ Box Runs
        ]

runsHelp :: String
runsHelp =
    "List Antithesis runs via the Moog proxy. Exit codes: \
    \0 success, 2 authorization or SSO failure, 3 proxy or network failure, \
    \4 invalid JSON from proxy."

runRuns :: IO Aeson.Value
runRuns = do
    proxyUrl <- proxyUrlFromEnv
    result <-
        runsRequestWithRefresh
            proxyUrl
            ensureToken
            (defaultTokenFile >>= evictCachedToken)
    case result of
        Right value -> pure value
        Left err -> do
            hPutStrLn stderr $ renderClientErr err
            exitWith $ ExitFailure $ clientErrExitCode err

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
