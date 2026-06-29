{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Reusable MPFS-v2 devnet lifecycle helpers shared by the
-- @moog-mpfs-v2-canary@ executable and the integration-test harness.
--
-- These helpers cover everything needed to stand up a throwaway MPFS
-- devnet around a freshly funded wallet: constructing the wallet,
-- pre-funding it by patching @shelley-genesis.json@'s @initialFunds@,
-- and launching the offchain @mpfs-devnet-server@. The lifecycle
-- /scenario/ (boot/request/update/end polling) is intentionally left to
-- the caller.
module MPFS.Devnet
    ( -- * Devnet configuration
      DevnetConfig (..)
    , loadDevnetConfig

      -- * Wallet
    , devnetWallet
    , rawWalletAddress

      -- * Genesis pre-funding
    , fundedGenesis
    , fundedGenesisMany
    , patchInitialFunds
    , patchInitialFundsMany
    , findDonor

      -- * Devnet server
    , withDevnetServer
    ) where

import Codec.Binary.Bech32 qualified as Bech32
import Control.Concurrent (threadDelay)
import Control.Exception
    ( bracket
    , catch
    )
import Control.Monad (forM_, unless, when)
import Core.Types.Basic (Address (..))
import Core.Types.Mnemonics (Mnemonics (..))
import Core.Types.Wallet (Wallet (..))
import Data.Aeson
    ( Value (..)
    , eitherDecodeFileStrict'
    , encodeFile
    )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Base16 qualified as Base16
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Scientific (floatingOrInteger)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client
    ( HttpException
    , defaultManagerSettings
    , httpNoBody
    , newManager
    , parseRequest
    , responseStatus
    )
import Network.HTTP.Types.Status (statusCode)
import Submitting
    ( walletFromMnemonic
    )
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesDirectoryExist
    , getFileSize
    , listDirectory
    , removeFile
    )
import System.Environment
    ( getEnvironment
    , lookupEnv
    )
import System.Exit (ExitCode, die)
import System.FilePath ((</>))
import System.IO
    ( Handle
    , IOMode (WriteMode)
    , withFile
    )
import System.Process
    ( CreateProcess (..)
    , ProcessHandle
    , StdStream (UseHandle)
    , createProcess
    , getProcessExitCode
    , proc
    , terminateProcess
    , waitForProcess
    )

-- | Everything needed to fund the genesis and launch the offchain
-- @mpfs-devnet-server@. The lifecycle scenario and any
-- @MOOG_CANARY_*@ tuning live with the caller, not here.
data DevnetConfig = DevnetConfig
    { serverBin :: FilePath
    -- ^ Path to the @mpfs-devnet-server@ executable.
    , mpfsBlueprint :: FilePath
    -- ^ Path to the MPFS Plutus blueprint.
    , genesisSource :: FilePath
    -- ^ Directory holding the pristine devnet genesis files.
    , devnetPath :: Maybe String
    -- ^ Optional directory prepended to @PATH@ for the server.
    , port :: Int
    -- ^ Port the devnet server listens on.
    }

-- | Read the devnet environment configuration:
-- @MPFS_DEVNET_SERVER@, @MPFS_BLUEPRINT@, @E2E_GENESIS_DIR@ (all
-- required) and @MPFS_DEVNET_PATH@ (optional). The listening port is
-- supplied by the caller so each consumer owns its own port policy.
loadDevnetConfig :: Int -> IO DevnetConfig
loadDevnetConfig port = do
    serverBin <- requireEnv "MPFS_DEVNET_SERVER"
    mpfsBlueprint <- requireEnv "MPFS_BLUEPRINT"
    genesisSource <- requireEnv "E2E_GENESIS_DIR"
    devnetPath <- lookupEnv "MPFS_DEVNET_PATH"
    pure
        DevnetConfig
            { serverBin
            , mpfsBlueprint
            , genesisSource
            , devnetPath
            , port
            }

requireEnv :: String -> IO String
requireEnv name = do
    value <- lookupEnv name
    case value of
        Just path -> pure path
        Nothing ->
            failTest
                $ name
                    <> " must be set"

-- | Build a wallet from a clear-text mnemonic phrase, aborting the
-- process if the mnemonic is invalid.
devnetWallet :: T.Text -> IO Wallet
devnetWallet mnemonic =
    case walletFromMnemonic False (ClearText mnemonic) of
        Right wallet -> pure wallet
        Left err -> failTest $ show err

-- | The raw (base16-encoded) bytes of a wallet's bech32 address, as
-- required by @shelley-genesis.json@'s @initialFunds@ keys.
rawWalletAddress :: Wallet -> IO T.Text
rawWalletAddress Wallet{address = Address address} = do
    case Bech32.decodeLenient address of
        Left err -> failTest $ show err
        Right (_, dataPart) ->
            case Bech32.dataPartToBytes dataPart of
                Nothing ->
                    failTest
                        $ "Could not decode wallet address bytes: "
                            <> T.unpack address
                Just bytes ->
                    pure $ TE.decodeUtf8 $ Base16.encode bytes

-- | Copy the pristine genesis into @tmp@ and pre-fund @wallet@ from the
-- largest existing donor, returning the path to the patched genesis
-- directory.
fundedGenesis
    :: FilePath
    -> DevnetConfig
    -> Wallet
    -> IO FilePath
fundedGenesis tmp cfg wallet = do
    rawAddress <- rawWalletAddress wallet
    fundedGenesisMany tmp cfg [rawAddress]

-- | Like 'fundedGenesis', but pre-fund every raw address in the list
-- (e.g. the distinct requester\/oracle\/agent role addresses). The
-- caller is responsible for de-duplicating the addresses.
fundedGenesisMany
    :: FilePath
    -> DevnetConfig
    -> [T.Text]
    -> IO FilePath
fundedGenesisMany tmp cfg rawAddresses = do
    let target = tmp </> "devnet-genesis"
    copyDirectoryRecursive cfg.genesisSource target
    patchInitialFundsMany
        (target </> "shelley-genesis.json")
        rawAddresses
        10_000_000_000_000
    pure target

-- | Move @walletLovelace@ from the richest @initialFunds@ donor to the
-- given raw address, rewriting @shelley-genesis.json@ in place.
patchInitialFunds
    :: FilePath
    -> T.Text
    -> Integer
    -> IO ()
patchInitialFunds genesisFile rawAddress =
    patchInitialFundsMany genesisFile [rawAddress]

-- | Move @walletLovelace@ to /each/ of @rawAddresses@, drawing the total
-- from the richest @initialFunds@ donor, rewriting
-- @shelley-genesis.json@ in place.
patchInitialFundsMany
    :: FilePath
    -> [T.Text]
    -> Integer
    -> IO ()
patchInitialFundsMany genesisFile rawAddresses walletLovelace = do
    value <- either error id <$> eitherDecodeFileStrict' genesisFile
    case value of
        Object root -> case KeyMap.lookup "initialFunds" root of
            Just (Object funds) -> do
                (donorKey, donorLovelace) <- findDonor funds
                let totalLovelace =
                        walletLovelace
                            * fromIntegral (length rawAddresses)
                when (donorLovelace <= totalLovelace)
                    $ failTest
                        "devnet genesis donor cannot fund the test wallets"
                let donorRemainder =
                        Number
                            $ fromInteger
                            $ donorLovelace - totalLovelace
                    walletFunds =
                        Number $ fromInteger walletLovelace
                    insertFund addr =
                        KeyMap.insert (Key.fromText addr) walletFunds
                    funds' =
                        KeyMap.insert donorKey donorRemainder
                            $ foldr insertFund funds rawAddresses
                    root' =
                        KeyMap.insert "initialFunds" (Object funds') root
                removeFile genesisFile
                encodeFile genesisFile $ Object root'
            _ ->
                failTest
                    "shelley-genesis.json has no initialFunds object"
        _ ->
            failTest
                "shelley-genesis.json root is not a JSON object"

-- | Find the @initialFunds@ entry holding the most lovelace.
findDonor :: KeyMap.KeyMap Value -> IO (Key.Key, Integer)
findDonor funds = do
    donors <- traverse readFund $ KeyMap.toList funds
    case donors of
        [] -> failTest "devnet genesis has no initial funds"
        _ -> pure $ maximumBy (comparing snd) donors
  where
    readFund (key, Number amount) =
        case floatingOrInteger amount of
            Right lovelace -> pure (key, lovelace)
            Left (_ :: Double) ->
                failTest
                    $ "genesis fund is not an integer: "
                        <> T.unpack (Key.toText key)
    readFund (key, _) =
        failTest
            $ "genesis fund is not numeric: "
                <> T.unpack (Key.toText key)

copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive source target = do
    createDirectoryIfMissing True target
    entries <- listDirectory source
    forM_ entries $ \entry -> do
        let from = source </> entry
            to = target </> entry
        isDirectory <- doesDirectoryExist from
        if isDirectory
            then copyDirectoryRecursive from to
            else copyFile from to

-- | Launch the @mpfs-devnet-server@ against @genesisDir@, wait until its
-- @\/status@ endpoint is healthy, run @action@ with the server host, and
-- tear the server down afterwards.
withDevnetServer
    :: FilePath
    -> DevnetConfig
    -> FilePath
    -> (String -> IO a)
    -> IO a
withDevnetServer tmp cfg genesisDir action = do
    env <- serverEnvironment cfg genesisDir
    let logFile = tmp </> "mpfs-devnet.log"
        host = "http://127.0.0.1:" <> show cfg.port
    withFile logFile WriteMode $ \logHandle ->
        bracket
            (startServer cfg env logHandle)
            (stopServer logFile)
            $ \processHandle -> do
                waitForStatus host logFile processHandle
                action host

serverEnvironment :: DevnetConfig -> FilePath -> IO [(String, String)]
serverEnvironment cfg genesisDir =
    setEnv "E2E_GENESIS_DIR" genesisDir
        . setEnv "MPFS_BLUEPRINT" cfg.mpfsBlueprint
        . maybe id prependPath cfg.devnetPath
        <$> getEnvironment

setEnv :: String -> String -> [(String, String)] -> [(String, String)]
setEnv key value env =
    (key, value) : filter ((/= key) . fst) env

prependPath :: String -> [(String, String)] -> [(String, String)]
prependPath prefix env =
    setEnv "PATH" (prefix <> ":" <> oldPath) env
  where
    oldPath = fromMaybe "" $ lookup "PATH" env

startServer
    :: DevnetConfig
    -> [(String, String)]
    -> Handle
    -> IO ProcessHandle
startServer cfg env logHandle = do
    (_, _, _, processHandle) <-
        createProcess
            (proc cfg.serverBin ["--port", show cfg.port])
                { env = Just env
                , std_out = UseHandle logHandle
                , std_err = UseHandle logHandle
                }
    pure processHandle

stopServer :: FilePath -> ProcessHandle -> IO ()
stopServer _logFile processHandle = do
    exitCode <- getProcessExitCode processHandle
    case exitCode of
        Just _ -> pure ()
        Nothing -> do
            terminateProcess processHandle
            _ <- waitForProcess processHandle
            pure ()

waitForStatus :: String -> FilePath -> ProcessHandle -> IO ()
waitForStatus host logFile processHandle = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest $ host <> "/status"
    let go attempt = do
            -- Check liveness first: if the server we just started has
            -- already exited (e.g. port still held by a stale process
            -- that fuser didn't catch in time), fail immediately rather
            -- than accepting a 200 from the old server.
            exitCode <- getProcessExitCode processHandle
            case exitCode of
                Just code -> failWithServerLog logFile code
                Nothing -> do
                    ready <-
                        ( do
                            response <- httpNoBody request manager
                            pure $ statusCode (responseStatus response) == 200
                        )
                            `catch` \(_ :: HttpException) -> pure False
                    if ready
                        then pure ()
                        else do
                            unless (attempt < 120)
                                $ failTestWithServerLog
                                    logFile
                                    "timed out waiting for /status"
                            threadDelay 1_000_000
                            go (attempt + 1)
    go (1 :: Int)

failWithServerLog :: FilePath -> ExitCode -> IO a
failWithServerLog logFile exitCode = do
    failTestWithServerLog logFile $ show exitCode

failTestWithServerLog :: FilePath -> String -> IO a
failTestWithServerLog logFile reason = do
    size <- getFileSize logFile
    contents <-
        if size == 0
            then pure "<empty log>"
            else readFile logFile
    failTest
        $ "mpfs-devnet-server exited: "
            <> reason
            <> "\n"
            <> contents

failTest :: String -> IO a
failTest = die
