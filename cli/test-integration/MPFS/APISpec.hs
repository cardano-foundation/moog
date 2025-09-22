{-# LANGUAGE OverloadedRecordDot #-}

module MPFS.APISpec (mpfsAPISpec)
where

import Cardano.Ledger.Alonzo.Tx (AlonzoTx)
import Cardano.Ledger.Api
    ( Addr (..)
    , ConwayEra
    , Datum (..)
    , EraTx (..)
    , EraTxBody (outputsTxBodyL)
    , binaryDataToData
    , eraProtVerLow
    , getPlutusData
    )
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (BabbageTxOut))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Binary
    ( DecCBOR (decCBOR)
    , decodeFullAnnotator
    )
import Cardano.Ledger.Core
    ( SafeToHash (originalBytes)
    , ScriptHash (..)
    )
import Cardano.Ledger.Credential (Credential (..))
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Lens (to, (^.))
import Control.Monad (void)
import Control.Monad.Catch (SomeException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Core.Context (withContext)
import Core.Types.Basic
    ( Owner (..)
    , TokenId (..)
    )
import Core.Types.CageDatum (CageDatum (..))
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Mnemonics (Mnemonics (..))
import Core.Types.Operation (Op (..), Operation (..))
import Core.Types.Tx
    ( TxHash
    , UnsignedTx (..)
    , WithTxHash (..)
    , WithUnsignedTx (WithUnsignedTx)
    )
import Core.Types.Wallet (Wallet (..))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Base16 (decode, encode)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Sequence.Strict qualified as Seq
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GitHub (Auth)
import MPFS.API
    ( RequestDeleteBody (RequestDeleteBody)
    , RequestInsertBody (RequestInsertBody)
    , RequestUpdateBody (RequestUpdateBody)
    , getToken
    , getTokenFacts
    , getTransaction
    , mpfsClient
    , requestDelete
    , requestInsert
    , requestUpdate
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
import PlutusTx (Data, fromData)
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
import Validation (mkValidation)

newtype UnencryptedWallet = UnencryptedWallet
    { _unencryptedMenmonics :: Text
    }

instance Aeson.FromJSON UnencryptedWallet where
    parseJSON = Aeson.withObject "UnencryptedWallet" $ \v -> do
        UnencryptedWallet <$> v Aeson..: "mnemonics"

mpfsPolicyId :: String
mpfsPolicyId = "c1e392ee7da9415f946de9d2aef9607322b47d6e84e8142ef0c340bf"

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
loadRequesterWallet = loadEnvWallet "ANTI_TEST_REQUESTER_WALLET"

loadOracleWallet :: IO Wallet
loadOracleWallet = loadEnvWallet "ANTI_TEST_ORACLE_WALLET"

loadAgentWallet :: IO Wallet
loadAgentWallet = loadEnvWallet "ANTI_TEST_AGENT_WALLET"

getHostFromEnv :: IO String
getHostFromEnv = getEnv "ANTI_MPFS_HOST"

newtype Call = Call {calling :: forall a. ClientM a -> IO a}

-- | CBOR deserialization of a tx in any era.
deserializeTx :: Text -> AlonzoTx ConwayEra
deserializeTx tx = case decode (T.encodeUtf8 tx) of
    Left err -> error $ "Failed to decode CBOR: " ++ show err
    Right bs -> case decodeFullAnnotator
        (eraProtVerLow @ConwayEra)
        "ConwayTx"
        decCBOR
        $ BL.fromStrict bs of
        Left err -> error $ "Failed to decode full CBOR: " ++ show err
        Right tx' -> tx'

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
            (mkValidation auth)
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
            (mkValidation auth)
            wait180S
            $ tokenCmdCore
            $ EndToken tokenId oracleWallet
    liftIO $ waitTx mpfs txHash

getFirstOutput :: AlonzoTx ConwayEra -> Maybe (String, Data)
getFirstOutput dtx = case dtx
    ^. bodyTxL
        . outputsTxBodyL
        . to (Seq.lookup 0) of
    Just
        ( BabbageTxOut
                (Addr Testnet (ScriptHashObj (ScriptHash addr)) _)
                _
                (Datum datum)
                _
            ) ->
            Just
                ( B.unpack $ encode $ originalBytes addr
                , getPlutusData $ binaryDataToData datum
                )
    _ -> error "No outputs found or output is not a BabbageTxOut"

(!?) :: [(JSString, JSValue)] -> String -> Maybe JSValue
(!?) = flip lookup . fmap (first fromJSString)

waitTx :: Call -> TxHash -> IO ()
waitTx (Call call) txHash = void $ go 600
  where
    go :: Int -> IO JSValue
    go 0 = error "Transaction not found after waiting"
    go n =
        do
            call (getTransaction txHash)
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
                    JSArray _ -> return ()
                    _ -> error "Response is not an object"
        it "can retrieve a request-insert tx"
            $ \Context{mpfs = Call call, tokenId, requesterWallet} -> do
                WithUnsignedTx (UnsignedTx tx) _ <-
                    call
                        $ requestInsert
                            (address requesterWallet)
                            tokenId
                        $ RequestInsertBody
                            (JSString "key")
                        $ JSString "value"
                let dtx = deserializeTx tx
                Just (policyId', datum) <- pure $ getFirstOutput dtx
                Just (Just (cageDatum :: CageDatum String (OpI String))) <-
                    pure $ fromData datum
                policyId' `shouldBe` mpfsPolicyId
                cageDatum
                    `shouldBe` RequestDatum
                        { tokenId = tokenId
                        , owner = requesterWallet.owner
                        , change =
                            Change
                                { key = Key "key"
                                , operation = Insert "value"
                                }
                        }
        it "can retrieve a request-delete tx"
            $ \Context{mpfs = Call call, tokenId, requesterWallet} -> do
                WithUnsignedTx (UnsignedTx tx) _ <-
                    call
                        $ requestDelete
                            requesterWallet.address
                            tokenId
                        $ RequestDeleteBody (JSString "key") (JSString "value")
                let dtx = deserializeTx tx
                Just (policyId', datum) <- pure $ getFirstOutput dtx
                Just (Just (cageDatum :: CageDatum String (OpD String))) <-
                    pure $ fromData datum
                policyId' `shouldBe` mpfsPolicyId
                cageDatum
                    `shouldBe` RequestDatum
                        { tokenId = tokenId
                        , owner = requesterWallet.owner
                        , change =
                            Change
                                { key = Key "key"
                                , operation = Delete "value"
                                }
                        }
        it "can retrieve a request-update tx"
            $ \Context{mpfs = Call call, tokenId, requesterWallet} -> do
                WithUnsignedTx (UnsignedTx tx) _ <-
                    call
                        $ requestUpdate
                            requesterWallet.address
                            tokenId
                        $ RequestUpdateBody
                            (JSString "key")
                            (JSString "oldValue")
                            (JSString "newValue")
                let dtx = deserializeTx tx
                Just (policyId', datum) <- pure $ getFirstOutput dtx
                Just (Just (cageDatum :: CageDatum String (OpU String String))) <-
                    pure $ fromData datum
                policyId' `shouldBe` mpfsPolicyId
                cageDatum
                    `shouldBe` RequestDatum
                        { tokenId = tokenId
                        , owner = requesterWallet.owner
                        , change =
                            Change
                                { key = Key "key"
                                , operation = Update "oldValue" "newValue"
                                }
                        }
