{-# LANGUAGE OverloadedRecordDot #-}

module MPFS.APISpec (spec)
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
import Cli (Command (..), cmd)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Lens (to, (^.))
import Control.Monad (void)
import Control.Monad.Catch (SomeException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Core.Types
    ( CageDatum (..)
    , Change (..)
    , Key (..)
    , Operation (..)
    , Owner (..)
    , Platform (..)
    , PublicKeyHash (PublicKeyHash)
    , RequestRefId (RequestRefId)
    , TokenId (..)
    , TxHash
    , UnsignedTx (..)
    , Username (..)
    , Wallet (..)
    , WithTxHash (..)
    , WithUnsignedTx (WithUnsignedTx)
    , textOf
    )
import Data.Bifunctor (first)
import Data.ByteString.Base16
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Sequence.Strict qualified as Seq
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import MPFS.API
    ( RequestDeleteBody (RequestDeleteBody)
    , RequestInsertBody (RequestInsertBody)
    , RequestUpdateBody (RequestUpdateBody)
    , getToken
    , getTokenFacts
    , getTransaction
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
import Oracle.Cli (OracleCommand (OracleTokenCommand), oracleCmd)
import Oracle.Token.Cli
    ( TokenCommand (..)
    , tokenCmdCore
    )
import PlutusTx (Data, fromData)
import Servant.Client (ClientM, mkClientEnv, parseBaseUrl, runClientM)
import Submitting (IfToWait (..), Submitting (..), readWallet)
import System.Environment (getEnv)
import Test.Hspec
    ( ActionWith
    , SpecWith
    , afterAll
    , beforeAll
    , describe
    , it
    , shouldBe
    )
import Text.JSON.Canonical
    ( JSString
    , JSValue (..)
    , ToJSON (..)
    , fromJSString
    )
import User.Requester.Cli (RequesterCommand (..), requesterCmd)
import User.Types (RegisterUserKey (..))

mpfsPolicyId :: String
mpfsPolicyId = "c1e392ee7da9415f946de9d2aef9607322b47d6e84e8142ef0c340bf"

host :: String
host = "https://mpfs.plutimus.com"

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
    , wait180 :: Submitting
    , tokenId :: TokenId
    , requesterWallet :: Wallet
    , oracleWallet :: Wallet
    }

setup :: IO Context
setup = do
    requesterWallet <- loadRequesterWallet
    oracleWallet <- loadOracleWallet
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
    oracle <- loadOracleWallet
    let wait180 :: Submitting
        wait180 = Submitting (Wait 180) $ \c -> do
            r <- runClientM c (mkClientEnv nm url)
            case r of
                Left err -> throwIO err
                Right res -> return res
    WithTxHash txHash mTokenId <- calling call $ do
        tokenCmdCore wait180 oracle Nothing BootToken
    liftIO $ waitTx call txHash
    case mTokenId of
        Nothing -> error "BootToken failed, no TokenId returned"
        Just tokenId ->
            return
                Context
                    { mpfs = call
                    , wait180
                    , tokenId
                    , requesterWallet
                    , oracleWallet
                    }

teardown :: ActionWith Context
teardown Context{mpfs, tokenId, wait180} = do
    wallet <- loadOracleWallet
    txHash <- calling mpfs $ do
        tokenCmdCore wait180 wallet (Just tokenId) EndToken
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

retractTx :: Submitting -> Wallet -> TxHash -> ClientM ()
retractTx wait180 wallet obj = do
    void
        $ cmd
            wait180
            (Right wallet)
            Nothing
        $ RetractRequest
            { outputReference =
                RequestRefId
                    $ textOf obj <> "-0"
            }

spec :: SpecWith ()
spec = do
    beforeAll setup $ afterAll teardown $ do
        describe "MPFS.API" $ do
            it "can retrieve config" $ \Context{mpfs = Call call, tokenId, oracleWallet} -> do
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
                        JSObject _obj -> return ()
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
                    Just (Just (cageDatum :: CageDatum String String)) <-
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
                    Just (Just (cageDatum :: CageDatum String String)) <-
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
                    Just (Just (cageDatum :: CageDatum String String)) <-
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

            it "can submit and retract a request-insert tx"
                $ \(Context{mpfs = Call call, wait180, tokenId}) -> do
                    wallet <- loadRequesterWallet
                    call $ do
                        insert <-
                            requesterCmd wait180 wallet tokenId
                                $ RegisterUser
                                $ RegisterUserKey
                                    { platform = Platform "test-platform"
                                    , username = Username "test-user"
                                    , pubkeyhash = PublicKeyHash "test-pubkeyhash"
                                    }
                        retractTx wait180 wallet insert
                        pure ()

            it "can update the anti token with a registered user"
                $ \(Context{mpfs = Call call, wait180, tokenId}) -> do
                    requester <- loadRequesterWallet
                    oracle <- loadOracleWallet
                    let key =
                            RegisterUserKey
                                { platform = Platform "test-platform"
                                , username = Username "test-user"
                                , pubkeyhash = PublicKeyHash "test-pubkeyhash"
                                }
                    keyJ <- toJSON key
                    call $ do
                        insertTx <-
                            requesterCmd wait180 requester tokenId
                                $ RegisterUser key
                        _updateInsertTx <-
                            oracleCmd wait180 oracle (Just tokenId)
                                $ OracleTokenCommand
                                $ UpdateToken
                                    [ RequestRefId
                                        $ textOf insertTx <> "-0"
                                    ]
                        facts <- getTokenFacts tokenId
                        liftIO
                            $ facts
                            `shouldBe` JSObject
                                [ ("key", keyJ)
                                , ("value", JSNull)
                                ]
                        deleteTx <-
                            requesterCmd wait180 requester tokenId
                                $ UnregisterUser key
                        _updateDeleteTx <-
                            oracleCmd wait180 oracle (Just tokenId)
                                $ OracleTokenCommand
                                $ UpdateToken
                                    [ RequestRefId
                                        $ textOf deleteTx <> "-0"
                                    ]
                        facts' <- getTokenFacts tokenId
                        liftIO $ facts' `shouldBe` JSObject []
                        pure ()

loadEnvWallet :: String -> IO Wallet
loadEnvWallet envVar = getEnv envVar >>= readWallet

loadRequesterWallet :: IO Wallet
loadRequesterWallet =
    loadEnvWallet "ANTI_TEST_REQUESTER_WALLET"

loadOracleWallet :: IO Wallet
loadOracleWallet = do
    loadEnvWallet "ANTI_TEST_ORACLE_WALLET"
