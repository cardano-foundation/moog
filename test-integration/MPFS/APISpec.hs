{-# LANGUAGE OverloadedRecordDot #-}

module MPFS.APISpec (mpfsAPISpec)
where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad.Catch (SomeException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Core.Context (withContext)
import Core.Types.Basic
    ( Owner (..)
    , TokenId (..)
    )
import Core.Types.Mnemonics (Mnemonics (..))
import Core.Types.Tx
    ( TxHash
    , WithTxHash (..)
    )
import Core.Types.Wallet (Wallet (..))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as B
import Data.Text (Text)
import Effects (mkEffects)
import GitHub (Auth)
import MPFS.API
    ( awaitTransactionV2
    , getToken
    , getTokenFacts
    , mpfsClient
    )
import Network.HTTP.Client
    ( ManagerSettings (managerResponseTimeout)
    , newManager
    , responseTimeoutMicro
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Oracle.Token.Cli
    ( TokenCommand (..)
    , tokenCmdCore
    )
import Oracle.Validate.Types (AValidationResult (ValidationSuccess))
import Servant.Client (ClientM, mkClientEnv, parseBaseUrl, runClientM)
import Submitting
    ( IfToWait (..)
    , Submission
    , Submitting (..)
    , readWallet
    , signAndSubmitMPFS
    )
import System.Environment (getEnv, lookupEnv)
import Test.Hspec
    ( ActionWith
    , SpecWith
    , aroundAllWith
    , describe
    , it
    , shouldBe
    )
import Text.JSON.Canonical
    ( JSString
    , JSValue (..)
    , fromJSString
    )

newtype UnencryptedWallet = UnencryptedWallet
    { _unencryptedMenmonics :: Text
    }

instance Aeson.FromJSON UnencryptedWallet where
    parseJSON = Aeson.withObject "UnencryptedWallet" $ \v -> do
        UnencryptedWallet <$> v Aeson..: "mnemonics"

loadEnvWallet :: String -> IO Wallet
loadEnvWallet envVar = do
    walletFile <- lookupEnv envVar
    case walletFile of
        Just file -> do
            content <- B.readFile file
            case Aeson.decodeStrict content of
                Just (UnencryptedWallet mnemonics) -> case readWallet (True, ClearText mnemonics) of
                    Left err ->
                        error
                            $ "Failed to read wallet from file "
                                ++ file
                                ++ ".\nError: "
                                ++ show err
                    Right wallet -> pure wallet
                Nothing ->
                    error
                        $ "Failed to decode wallet file at "
                            ++ file
                            ++ ".\n \
                               \ Please ensure it is a valid funded preprod wallet file."
        Nothing ->
            error
                $ "Environment variable "
                    ++ envVar
                    ++ " is not set.\n \
                       \ Please set it to a valid funded preprod wallet file path.\n \
                       \ You can reuse the same wallet if you do not want to separate roles.\n \
                       \ You can create a wallet with the command:\n \
                       \ > wallet create <filename>\n"

loadRequesterWallet :: IO Wallet
loadRequesterWallet = loadEnvWallet "MOOG_TEST_REQUESTER_WALLET"

loadOracleWallet :: IO Wallet
loadOracleWallet = loadEnvWallet "MOOG_TEST_ORACLE_WALLET"

loadAgentWallet :: IO Wallet
loadAgentWallet = loadEnvWallet "MOOG_TEST_AGENT_WALLET"

getHostFromEnv :: IO String
getHostFromEnv = getEnv "MOOG_MPFS_HOST"

newtype Call = Call {calling :: forall a. ClientM a -> IO a}

data Context = Context
    { mpfs :: Call
    , wait180S :: Wallet -> Submission ClientM
    , tokenId :: TokenId
    , requesterWallet :: Wallet
    , oracleWallet :: Wallet
    , agentWallet :: Wallet
    , auth :: Auth
    }

setup :: Auth -> IO Context
setup auth = do
    requesterWallet <- loadRequesterWallet
    oracleWallet <- loadOracleWallet
    agentWallet <- loadAgentWallet
    host <- getHostFromEnv
    url <- parseBaseUrl host
    nm <-
        newManager
            $ tlsManagerSettings
                { managerResponseTimeout = responseTimeoutMicro $ 90 * 1000000
                }
    let call :: Call
        call = Call $ \f -> do
            r <- runClientM f (mkClientEnv nm url)
            case r of
                Left err -> throwIO err
                Right res -> return res
    let wait180 :: Submitting
        wait180 = Submitting (Wait 180) $ \c -> do
            r <- runClientM c (mkClientEnv nm url)
            case r of
                Left err -> throwIO err
                Right res -> return res
        wait180S = signAndSubmitMPFS wait180
    ValidationSuccess (WithTxHash txHash mTokenId) <- calling call $ do
        withContext
            mpfsClient
            (mkEffects auth)
            wait180S
            $ tokenCmdCore
            $ BootToken oracleWallet
    liftIO $ waitTx call txHash
    case mTokenId of
        Nothing -> error "BootToken failed, no TokenId returned"
        Just tokenId ->
            return
                Context
                    { mpfs = call
                    , wait180S
                    , tokenId
                    , requesterWallet
                    , oracleWallet
                    , agentWallet
                    , auth
                    }

teardown :: Auth -> ActionWith Context
teardown auth Context{mpfs, tokenId, wait180S, oracleWallet} = do
    txHash <- calling mpfs $ do
        withContext
            mpfsClient
            (mkEffects auth)
            wait180S
            $ tokenCmdCore
            $ EndToken tokenId oracleWallet
    liftIO $ waitTx mpfs txHash

(!?) :: [(JSString, JSValue)] -> String -> Maybe JSValue
(!?) = flip lookup . fmap (first fromJSString)

waitTx :: Call -> TxHash -> IO ()
waitTx (Call call) txHash = go (600 :: Int)
  where
    go :: Int -> IO ()
    go 0 = error "Transaction not found after waiting"
    go n =
        call (awaitTransactionV2 txHash)
            `catch` \(_ :: SomeException) -> do
                liftIO $ threadDelay 1000000
                go (n - 1)

setupAction :: ActionWith Context -> ActionWith Auth
setupAction action auth = do
    ctx <- setup auth
    action ctx
    teardown auth ctx

mpfsAPISpec :: SpecWith Auth
mpfsAPISpec = aroundAllWith setupAction $ do
    describe "MPFS.API" $ do
        it "can retrieve config"
            $ \Context{mpfs = Call call, tokenId, oracleWallet} -> do
                res <- call $ getToken tokenId
                case res of
                    JSObject obj -> case obj !? "state" of
                        Just (JSObject state) -> case state !? "owner" of
                            Just (JSString antiTokenOwner') ->
                                Owner (fromJSString antiTokenOwner')
                                    `shouldBe` oracleWallet.owner
                            _ -> error "Field 'owner' is missing or not a string"
                        _ -> error "Field 'state' is missing or not an object"
                    _ -> error "Response is not an object"
        it "can retrieve token facts"
            $ \(Context{mpfs = Call call, tokenId}) -> do
                res <- call $ getTokenFacts tokenId
                case res of
                    JSObject _ -> return ()
                    _ -> error "Response is not an object"

-- TODO(#178): restore + migrate request-tx specs to the *FromFacts v2 API.
-- The following three specs were dropped during the #177 v2 compile-fix
-- (they build + inspect unsigned request-tx datums, which is oracle
-- request-flow behavior owned by #178, not the self-hosted-devnet proof):
--   "can retrieve a request-insert tx"
--   "can retrieve a request-delete tx"
--   "can retrieve a request-update tx"
