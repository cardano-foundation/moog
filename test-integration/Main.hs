{-# LANGUAGE NumericUnderscores #-}

import Agent.ValidationSpec (agentValidationSpec)
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
import GitHub (Auth (..))
import Lib.GitHubSpec (githubSpec)
import Lib.Github.OracleValidationSpec
    ( existenceSpec
    , roleSpecs
    , userSpec
    , vkeySpec
    )
import MPFS.APISpec (mpfsAPISpec)
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
import Submitting (readWallet)
import System.Environment (lookupEnv, setEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (beforeAll, hspec)

-- | The integration suite stands up its own MPFS: it launches the
-- offchain @mpfs-devnet-server@ against a genesis that pre-funds every
-- distinct role wallet, points @MOOG_MPFS_HOST@ at it, waits for the
-- indexer to report ready, then runs the specs. No external
-- docker-compose @mpfs@\/@yaci@ is required.
main :: IO ()
main = do
    wallets <- traverse loadEnvWallet roleWalletEnvVars
    rawAddresses <- nub <$> traverse rawWalletAddress wallets
    port <- maybe 38_091 read <$> lookupEnv "MPFS_PORT"
    withSystemTempDirectory "moog-itest-devnet" $ \tmp -> do
        cfg <- loadDevnetConfig port
        genesisDir <- fundedGenesisMany tmp cfg rawAddresses
        withDevnetServer tmp cfg genesisDir $ \host -> do
            setEnv "MOOG_MPFS_HOST" host
            setEnv "MPFS_PORT" $ show port
            waitForSnapshotReady host
            hspec $ do
                beforeAll getPAT $ do
                    githubSpec
                    existenceSpec
                    roleSpecs
                    mpfsAPISpec
                    vkeySpec
                userSpec
                agentValidationSpec

-- | The role wallets the suite loads. CI commonly points all three at
-- the same @wallet.json@; the addresses are de-duplicated before
-- funding.
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

-- | Load a role wallet exactly as the spec does (mnemonics file +
-- 'readWallet' with the same encrypted flag), so the funded genesis
-- address matches the address the spec signs with.
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
-- once present the token-state reads the suite needs are serviceable.
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
