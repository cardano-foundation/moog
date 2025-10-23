{-
This is a full automated oracle process that can be used in the CLI.
The process
- respects a maximum number of requests to process in one run.
- poll the MPFS system to get new requests. (get token)
- batches requests and calls MPFS to include them in the token state.
-}
{-# LANGUAGE QuasiQuotes #-}

module Oracle.Process
    ( ProcessOptions (..)
    , oracleProcess
    , parseArgs
    , pollIntervalOption
    ) where

import Cli (Command (GetToken), WithValidation (..), cmd)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Core.Context (WithContext, withContext)
import Core.Options (tokenIdOption, walletOption)
import Core.Types.Basic (RequestRefId, TokenId)
import Core.Types.MPFS (MPFSClient (..), mpfsClientOption)
import Core.Types.Wallet (Wallet)
import Data.Functor (void)
import Data.String.QQ (s)
import Effects (mkEffects)
import GitHub (Auth)
import MPFS.API (mpfsClient)
import OptEnvConf
import Options (githubAuthOption, secretsFileOption)
import Oracle.Token.Cli
    ( TokenCommand (..)
    , TokenUpdateFailure
    , tokenCmdCore
    )
import Oracle.Types
    ( Token (tokenRequests)
    , requestZooRefId
    )
import Oracle.Validate.Types (AValidationResult (..), Validated (..))
import Paths_moog (version)

intro :: String
intro =
    [s|
    Cardano Antithesis Oracle Process

    This process will run indefinitely, polling the MPFS system for new requests
    to include in the oracle token. It will batch requests according to the
    maximum number of requests per batch specified, and will include them in
    the token state.

    To stop the process, simply interrupt it (Ctrl+C).

    To get help on the available options, use the --help flag.

    To get bash cli completion use

    > source <(moog-oracle --bash-completion-script "$(which moog-oracle)")

    Fish and zsh completions are also available.
    |]

parseArgs :: IO ProcessOptions
parseArgs =
    runParser
        version
        intro
        $ withYamlConfig
            secretsFileOption
            processOptionsParser

data ProcessOptions = ProcessOptions
    { poAuth :: Auth
    , poMaxRequestsPerBatch :: Int
    , poPollIntervalSeconds :: Int
    , poWallet :: Wallet
    , poTokenId :: TokenId
    , poMPFSClient :: MPFSClient
    }

processOptionsParser :: Parser ProcessOptions
processOptionsParser =
    ProcessOptions
        <$> githubAuthOption
        <*> maxRequestsPerBatchOption
        <*> pollIntervalOption
        <*> walletOption
        <*> tokenIdOption
        <*> mpfsClientOption

maxRequestsPerBatchOption :: Parser Int
maxRequestsPerBatchOption =
    setting
        [ long "max-requests-per-batch"
        , metavar "INT"
        , help "Maximum number of requests to include in one batch"
        , env "MAX_REQUESTS_PER_BATCH"
        , reader auto
        , option
        , value 5
        ]

pollIntervalOption :: Parser Int
pollIntervalOption =
    setting
        [ long "poll-interval"
        , metavar "SECONDS"
        , help "Interval in seconds between polling for new requests"
        , env "POLL_INTERVAL_SECONDS"
        , reader auto
        , option
        , value 30
        ]

oracleProcess :: ProcessOptions -> IO ()
oracleProcess = \case
    opts@ProcessOptions{poAuth, poMPFSClient} -> do
        let MPFSClient{runMPFS, submitTx} = poMPFSClient
        runMPFS
            $ withContext
                mpfsClient
                (mkEffects poAuth)
                submitTx
            $ processServer opts

processServer
    :: forall m
     . MonadIO m
    => ProcessOptions
    -> WithContext m ()
processServer opts@ProcessOptions{poPollIntervalSeconds} = do
    liftIO $ putStrLn "Starting oracle process server..."
    let waitLoop = do
            liftIO
                $ putStrLn
                $ "Sleeping for "
                    ++ show poPollIntervalSeconds
                    ++ " seconds..."
            liftIO $ threadDelay (poPollIntervalSeconds * 1000000)
            loop

        loop :: WithContext m () = do
            liftIO $ putStrLn "Polling for new requests..."
            reqIds <- liftIO $ poll opts
            if null reqIds
                then do
                    liftIO
                        $ putStrLn "No new requests found."
                    waitLoop
                else do
                    liftIO
                        $ putStrLn
                        $ "Found "
                            ++ show (length reqIds)
                            ++ " new requests. Processing..."
                    let batches = batch opts reqIds
                    forM_ batches $ \batchReqIds -> do
                        liftIO
                            $ putStrLn
                            $ "Submitting batch of "
                                ++ show (length batchReqIds)
                                ++ " requests..."
                        result <- submit opts batchReqIds
                        case result of
                            ValidationFailure err ->
                                liftIO
                                    $ putStrLn
                                    $ "Failed to submit batch: " ++ show err
                            ValidationSuccess txHash ->
                                liftIO
                                    $ putStrLn
                                    $ "Successfully submitted batch with tx hash: "
                                        ++ show txHash
                    waitLoop
    loop
poll :: ProcessOptions -> IO [RequestRefId]
poll ProcessOptions{poTokenId, poMPFSClient, poAuth} = do
    result <- cmd (GetToken poAuth poMPFSClient poTokenId)
    case result of
        ValidationFailure err -> error $ "Failed to get token: " ++ show err
        ValidationSuccess token -> do
            print $ tokenRequests token
            pure
                $ fmap (requestZooRefId . request)
                $ flip filter (tokenRequests token)
                $ \(WithValidation v _) -> case v of
                    ValidationFailure _err -> False
                    ValidationSuccess Validated -> True

batch :: ProcessOptions -> [RequestRefId] -> [[RequestRefId]]
batch ProcessOptions{poMaxRequestsPerBatch} = go
  where
    go [] = []
    go xs = take poMaxRequestsPerBatch xs : go (drop poMaxRequestsPerBatch xs)

submit
    :: Monad m
    => ProcessOptions
    -> [RequestRefId]
    -> WithContext
        m
        ( AValidationResult
            TokenUpdateFailure
            ()
        )
submit ProcessOptions{poWallet, poTokenId} reqIds = do
    void <$> tokenCmdCore (UpdateToken poTokenId poWallet reqIds)
