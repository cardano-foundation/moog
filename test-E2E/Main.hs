{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Core.Types.Mnemonics (Mnemonics (ClearText))
import Core.Types.Wallet (Wallet)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.List (nub)
import Data.Text (Text)
import E2ESpec (e2eSpec)
import GitHub (Auth (..))
import MPFS.Devnet
    ( fundedGenesisMany
    , loadDevnetConfig
    , rawWalletAddress
    , withDevnetServer
    )
import Network.HTTP.Client
    ( defaultManagerSettings
    , httpLbs
    , newManager
    , parseRequest
    , responseBody
    )
import Network.Socket
    ( AddrFamily (..)
    , SocketType (..)
    , bind
    , close
    , defaultProtocol
    , getSocketName
    , socket
    )
import Network.Socket (SockAddr (..))
import Submitting (readWallet)
import System.Environment (lookupEnv, setEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (beforeAll, hspec)

-- | The e2e suite stands up its own MPFS: it launches the offchain
-- @mpfs-devnet-server@ against a genesis that pre-funds every distinct
-- role wallet the scenarios sign with, points @MOOG_MPFS_HOST@ at it,
-- waits for the indexer to report a snapshot, then runs the scenario
-- specs. No external docker-compose @mpfs@\/@yaci@ is required; the
-- requester\/oracle CLI flows are fully hermetic (no agent\/Antithesis
-- network calls).
-- | Ask the OS for a free TCP port by binding to 0, reading the
-- assigned port, and immediately closing the socket.  There is a
-- small TOCTOU window, but in practice the OS re-uses the port only
-- after a delay, so the server will bind it successfully.
getFreePort :: IO Int
getFreePort = do
    s <- socket AF_INET Stream defaultProtocol
    bind s (SockAddrInet 0 0)
    SockAddrInet port _ <- getSocketName s
    close s
    pure (fromIntegral port)

main :: IO ()
main = do
    wallets <- traverse loadEnvWallet roleWalletEnvVars
    rawAddresses <- nub <$> traverse rawWalletAddress wallets
    -- Use MPFS_PORT if set (allows manual override), otherwise ask the
    -- OS for a free port.  A fresh port each run avoids connecting to a
    -- stale mpfs-devnet-server left behind by a cancelled CI job.
    port <- maybe getFreePort (pure . read) =<< lookupEnv "MPFS_PORT"
    withSystemTempDirectory "moog-e2e-devnet" $ \tmp -> do
        cfg <- loadDevnetConfig port
        genesisDir <- fundedGenesisMany tmp cfg rawAddresses
        withDevnetServer tmp cfg genesisDir $ \host -> do
            setEnv "MOOG_MPFS_HOST" host
            setEnv "MPFS_PORT" $ show port
            waitForSnapshotReady host
            hspec $ do
                beforeAll getPAT $ do
                    e2eSpec

-- | The role wallets the scenarios sign with. CI commonly points all
-- three at the same @wallet.json@ (the GitHub-registered @cfhal@
-- wallet); the addresses are de-duplicated before funding.
roleWalletEnvVars :: [String]
roleWalletEnvVars =
    [ "MOOG_TEST_REQUESTER_WALLET"
    , "MOOG_TEST_ORACLE_WALLET"
    , "MOOG_TEST_AGENT_WALLET"
    ]

newtype UnencryptedWallet = UnencryptedWallet Text

instance FromJSON UnencryptedWallet where
    parseJSON =
        withObject "UnencryptedWallet" $ \v ->
            UnencryptedWallet <$> v .: "mnemonics"

-- | Load a role wallet exactly as the scenarios do (mnemonics file +
-- 'readWallet' with the same encrypted flag), so the funded genesis
-- address matches the address the @moog@ CLI signs with.
loadEnvWallet :: String -> IO Wallet
loadEnvWallet envVar = do
    walletFile <- lookupEnv envVar
    file <-
        maybe
            (error $ envVar <> " is not set; point it at a wallet file")
            pure
            walletFile
    content <- BC.readFile file
    case Aeson.decodeStrict content of
        Just (UnencryptedWallet mnemonics) ->
            case readWallet (True, ClearText mnemonics) of
                Right wallet -> pure wallet
                Left err ->
                    error
                        $ "Failed to read wallet from "
                            <> file
                            <> ": "
                            <> show err
        Nothing ->
            error $ "Failed to decode wallet file at " <> file

-- | Poll @GET \/status@ until the indexer has computed a CSMT snapshot,
-- i.e. the response carries a present, non-null top-level @utxo_root@.
-- That field is absent\/null while the indexer is still warming up;
-- once present the token-state reads the scenarios need are
-- serviceable.
waitForSnapshotReady :: String -> IO ()
waitForSnapshotReady host = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest $ host <> "/status"
    let ready =
            ( do
                response <- httpLbs request manager
                pure $ snapshotReady $ responseBody response
            )
                `catch` \(_ :: SomeException) -> pure False
        go 0 =
            error
                "mpfs-devnet-server /status never reported a utxo_root"
        go n = do
            isReady <- ready
            unless isReady $ do
                threadDelay 1_000_000
                go (n - 1 :: Int)
    go 180

-- | @True@ when a @GET \/status@ body carries a present, non-null
-- top-level @utxo_root@.
snapshotReady :: BL.ByteString -> Bool
snapshotReady body = case Aeson.decode body of
    Just (Aeson.Object o) -> case KeyMap.lookup "utxo_root" o of
        Nothing -> False
        Just Aeson.Null -> False
        Just _ -> True
    _ -> False

tryGetPAT :: IO (Maybe Auth)
tryGetPAT = fmap (OAuth . BC.pack) <$> lookupEnv "MOOG_GITHUB_PAT"

getPAT :: IO Auth
getPAT = do
    mpat <- tryGetPAT
    case mpat of
        Just pat -> return pat
        Nothing ->
            error
                "Environment variable MOOG_GITHUB_PAT is not set. \
                \ Please set it to some valid GitHub Personal Access Token with \
                \ read access to public repositories."
