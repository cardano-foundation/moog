{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Codec.Binary.Bech32 qualified as Bech32
import Control.Concurrent (threadDelay)
import Control.Exception
    ( bracket
    , bracket_
    , catch
    )
import Control.Monad (forM_, unless, when)
import Core.Types.Basic
    ( Address (..)
    , TokenId (..)
    )
import Core.Types.MPFS
    ( newClient
    )
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
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Scientific (floatingOrInteger)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import MPFS.Canary
    ( EndBoundaryCanaryResult (..)
    , bootThenEndCanary
    )
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
    ( IfToWait (..)
    , walletFromMnemonic
    )
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesPathExist
    , getFileSize
    , listDirectory
    , removeFile
    , removePathForcibly
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
import System.IO.Temp (withSystemTempDirectory)
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
import Text.JSON.Canonical
    ( ToJSON (toJSON)
    , renderCanonicalJSON
    )

data CanaryConfig = CanaryConfig
    { serverBin :: FilePath
    , mpfsBlueprint :: FilePath
    , genesisSource :: FilePath
    , devnetPath :: Maybe String
    , port :: Int
    , polls :: Int
    }

main :: IO ()
main = do
    cfg <- loadConfig
    result <- runCanary cfg
    assertCanaryResult result
    output <- toJSON result
    BL.putStrLn $ renderCanonicalJSON output

runCanary :: CanaryConfig -> IO EndBoundaryCanaryResult
runCanary cfg =
    withCanaryTempDirectory $ \tmp -> do
        wallet <- canaryWallet
        genesisDir <- fundedGenesis tmp cfg wallet
        withDevnetServer tmp cfg genesisDir $ \host -> do
            client <- newClient (host, NoWait, 120)
            bootThenEndCanary client wallet cfg.polls

assertCanaryResult :: EndBoundaryCanaryResult -> IO ()
assertCanaryResult result = do
    let TokenId tokenId = result.canaryTokenId
    when (null tokenId)
        $ failTest "end boundary canary returned an empty token id"
    when (result.bootTransactionPolls < 0)
        $ failTest
            "end boundary canary returned a negative boot transaction poll count"
    when (result.tokenBeforeEndPolls < 0)
        $ failTest
            "end boundary canary returned a negative token-before-end poll count"
    when (result.endTransactionPolls < 0)
        $ failTest
            "end boundary canary returned a negative end transaction poll count"
    when (result.tokenGonePolls < 0)
        $ failTest
            "end boundary canary returned a negative token-gone poll count"

loadConfig :: IO CanaryConfig
loadConfig = do
    serverBin <- requireEnv "MPFS_DEVNET_SERVER"
    mpfsBlueprint <- requireEnv "MPFS_BLUEPRINT"
    genesisSource <- requireEnv "E2E_GENESIS_DIR"
    devnetPath <- lookupEnv "MPFS_DEVNET_PATH"
    port <- readEnv "MOOG_MPFS_V2_CANARY_PORT" 38_081
    polls <- readEnv "MOOG_CANARY_POLLS" 240
    pure
        CanaryConfig
            { serverBin
            , mpfsBlueprint
            , genesisSource
            , devnetPath
            , port
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

requireEnv :: String -> IO String
requireEnv name = do
    value <- lookupEnv name
    case value of
        Just path -> pure path
        Nothing ->
            failTest
                $ name
                    <> " must be set for moog-mpfs-v2-canary"

readEnv :: Read a => String -> a -> IO a
readEnv name fallback = do
    value <- lookupEnv name
    pure $ maybe fallback read value

canaryWallet :: IO Wallet
canaryWallet =
    case walletFromMnemonic False (ClearText mnemonic) of
        Right wallet -> pure wallet
        Left err -> failTest $ show err
  where
    mnemonic =
        "culture island clump online fatigue curve fish during mandate echo cradle cat arrange upset region"

fundedGenesis
    :: FilePath
    -> CanaryConfig
    -> Wallet
    -> IO FilePath
fundedGenesis tmp cfg wallet = do
    let target = tmp </> "devnet-genesis"
    copyDirectoryRecursive cfg.genesisSource target
    rawAddress <- rawWalletAddress wallet
    patchInitialFunds
        (target </> "shelley-genesis.json")
        rawAddress
        10_000_000_000_000
    pure target

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

patchInitialFunds
    :: FilePath
    -> T.Text
    -> Integer
    -> IO ()
patchInitialFunds genesisFile rawAddress walletLovelace = do
    value <- either error id <$> eitherDecodeFileStrict' genesisFile
    case value of
        Object root -> case KeyMap.lookup "initialFunds" root of
            Just (Object funds) -> do
                (donorKey, donorLovelace) <- findDonor funds
                when (donorLovelace <= walletLovelace)
                    $ failTest
                        "devnet genesis donor cannot fund canary wallet"
                let donorRemainder =
                        Number $ fromInteger $ donorLovelace - walletLovelace
                    walletFunds =
                        Number $ fromInteger walletLovelace
                    funds' =
                        KeyMap.insert donorKey donorRemainder
                            $ KeyMap.insert
                                (Key.fromText rawAddress)
                                walletFunds
                                funds
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

withDevnetServer
    :: FilePath
    -> CanaryConfig
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

serverEnvironment :: CanaryConfig -> FilePath -> IO [(String, String)]
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
    :: CanaryConfig
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
            ready <-
                ( do
                    response <- httpNoBody request manager
                    pure $ statusCode (responseStatus response) == 200
                )
                    `catch` \(_ :: HttpException) -> pure False
            if ready
                then pure ()
                else do
                    exitCode <- getProcessExitCode processHandle
                    case exitCode of
                        Just code -> failWithServerLog logFile code
                        Nothing -> do
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
