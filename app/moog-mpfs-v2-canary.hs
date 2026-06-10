{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Exception (bracket_)
import Control.Monad (unless, when)
import Core.Types.Basic
    ( TokenId (..)
    )
import Core.Types.MPFS
    ( newClient
    )
import Core.Types.Wallet (Wallet)
import Data.ByteString.Lazy.Char8 qualified as BL
import MPFS.Canary
    ( FullLifecycleCanaryResult (..)
    , fullLifecycleCanary
    )
import MPFS.Devnet
    ( DevnetConfig
    , devnetWallet
    , fundedGenesis
    , loadDevnetConfig
    , withDevnetServer
    )
import Submitting
    ( IfToWait (..)
    )
import System.Directory
    ( createDirectoryIfMissing
    , doesPathExist
    , removePathForcibly
    )
import System.Environment
    ( lookupEnv
    )
import System.Exit (die)
import System.IO.Temp (withSystemTempDirectory)
import Text.JSON.Canonical
    ( ToJSON (toJSON)
    , renderCanonicalJSON
    )

data CanaryConfig = CanaryConfig
    { devnet :: DevnetConfig
    , polls :: Int
    }

main :: IO ()
main = do
    cfg <- loadConfig
    result <- runCanary cfg
    assertCanaryResult result
    output <- toJSON result
    BL.putStrLn $ renderCanonicalJSON output

runCanary :: CanaryConfig -> IO FullLifecycleCanaryResult
runCanary cfg =
    withCanaryTempDirectory $ \tmp -> do
        wallet <- canaryWallet
        genesisDir <- fundedGenesis tmp cfg.devnet wallet
        withDevnetServer tmp cfg.devnet genesisDir $ \host -> do
            client <- newClient (host, NoWait, 120)
            fullLifecycleCanary client wallet cfg.polls

assertCanaryResult :: FullLifecycleCanaryResult -> IO ()
assertCanaryResult result = do
    let TokenId tokenId = result.canaryTokenId
    when (null tokenId)
        $ failTest "full lifecycle canary returned an empty token id"
    when (result.bootTransactionPolls < 0)
        $ failTest
            "full lifecycle canary returned a negative boot transaction poll count"
    when (result.requestInsertTransactionPolls < 0)
        $ failTest
            "full lifecycle canary returned a negative request insert transaction poll count"
    when (result.requestObservedPolls < 0)
        $ failTest
            "full lifecycle canary returned a negative request observed poll count"
    when (result.updateTokenTransactionPolls < 0)
        $ failTest
            "full lifecycle canary returned a negative update token transaction poll count"
    when (result.factObservedPolls < 0)
        $ failTest
            "full lifecycle canary returned a negative fact observed poll count"
    when (result.tokenBeforeEndPolls < 0)
        $ failTest
            "full lifecycle canary returned a negative token-before-end poll count"
    when (result.endTransactionPolls < 0)
        $ failTest
            "full lifecycle canary returned a negative end transaction poll count"
    when (result.tokenGonePolls < 0)
        $ failTest
            "full lifecycle canary returned a negative token-gone poll count"

loadConfig :: IO CanaryConfig
loadConfig = do
    port <- readEnv "MOOG_MPFS_V2_CANARY_PORT" 38_081
    devnet <- loadDevnetConfig port
    polls <- readEnv "MOOG_CANARY_POLLS" 240
    pure
        CanaryConfig
            { devnet
            , polls
            }

withCanaryTempDirectory :: (FilePath -> IO a) -> IO a
withCanaryTempDirectory action = do
    configured <- lookupEnv "MOOG_CANARY_TMPDIR"
    case configured of
        Nothing -> withSystemTempDirectory "moog-mpfs-v2-canary" action
        Just tmp -> do
            keepTmp <- (== Just "1") <$> lookupEnv "MOOG_CANARY_KEEP_TMP"
            bracket_
                (prepareConfiguredTemp tmp)
                (unless keepTmp $ removePathForcibly tmp)
                (action tmp)

prepareConfiguredTemp :: FilePath -> IO ()
prepareConfiguredTemp tmp = do
    exists <- doesPathExist tmp
    when exists $ removePathForcibly tmp
    createDirectoryIfMissing True tmp

readEnv :: Read a => String -> a -> IO a
readEnv name fallback = do
    value <- lookupEnv name
    pure $ maybe fallback read value

canaryWallet :: IO Wallet
canaryWallet = devnetWallet mnemonic
  where
    mnemonic =
        "culture island clump online fatigue curve fish during mandate echo cradle cat arrange upset region"

failTest :: String -> IO a
failTest = die
