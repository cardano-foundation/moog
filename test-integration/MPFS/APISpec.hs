{-# LANGUAGE OverloadedRecordDot #-}

module MPFS.APISpec (mpfsAPISpec)
where

import Cardano.Ledger.Api
    ( ConwayEra
    , Datum (..)
    , EraTx (..)
    , EraTxBody (outputsTxBodyL)
    , binaryDataToData
    , eraProtVerLow
    , getPlutusData
    )
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (BabbageTxOut))
import Cardano.Ledger.Binary
    ( DecCBOR (decCBOR)
    , decodeFullAnnotator
    )
import Cardano.Tx.Ledger (ConwayTx)
import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Lens ((^.))
import Control.Monad.Catch (SomeException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Core.Context (withContext)
import Core.Types.Basic
    ( Owner (..)
    , RequestRefId
    , TokenId (..)
    )
import Core.Types.CageDatum (CageDatum (..))
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..), parseFacts)
import Core.Types.Mnemonics (Mnemonics (..))
import Core.Types.Operation (Op (..), Operation (..))
import Core.Types.Tx
    ( Root
    , TxHash
    , UnsignedTx (..)
    , WithTxHash (..)
    , WithUnsignedTx (..)
    )
import Core.Types.Wallet (Wallet (..))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Base16 (decode)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Effects (mkEffects)
import GitHub (Auth)
import MPFS.API
    ( RequestDeleteBody (..)
    , RequestInsertBody (..)
    , RequestUpdateBody (..)
    , awaitTransactionV2
    , getToken
    , getTokenFacts
    , mpfsClient
    , requestDeleteFromFacts
    , requestInsertFromFacts
    , requestUpdateFromFacts
    )
import MPFS.Canary (canaryBootParams)
import Network.HTTP.Client
    ( ManagerSettings (managerResponseTimeout)
    , newManager
    , responseTimeoutMicro
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Oracle.Config.Cli (ConfigCmd (..), configCmd)
import Oracle.Config.Types
    ( Config
    , ConfigKey
    , mkCurrentConfig
    )
import Oracle.Token.Cli
    ( TokenCommand (..)
    , TokenRejectFailure
    , tokenCmdCore
    )
import Oracle.Types
    ( RequestZoo (..)
    , Token (..)
    , TokenState (..)
    , requestZooRefId
    )
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Types (AValidationResult (..))
import PlutusTx (Data (..), FromData, fromData)
import Servant.Client (ClientM, mkClientEnv, parseBaseUrl, runClientM)
import Submitting
    ( IfToWait (..)
    , Submission (..)
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
    , expectationFailure
    , it
    , shouldBe
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSString
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

-- | Decode a hex-encoded Conway transaction body for inspection.
deserializeTx :: Text -> ConwayTx
deserializeTx tx = case decode (T.encodeUtf8 tx) of
    Left err -> error $ "Failed to decode CBOR: " ++ show err
    Right bs ->
        case decodeFullAnnotator
            (eraProtVerLow @ConwayEra)
            "ConwayTx"
            decCBOR
            (BL.fromStrict bs) of
            Left err -> error $ "Failed to decode full CBOR: " ++ show err
            Right tx' -> tx'

-- | The first transaction output carrying an inline datum: the cage
-- request output the @*FromFacts@ builders lock the 'RequestDatum' at.
-- Change\/refund outputs carry no datum and are skipped.
firstInlineDatum :: ConwayTx -> Maybe Data
firstInlineDatum dtx =
    case
        [ getPlutusData (binaryDataToData bd)
        | BabbageTxOut _ _ (Datum bd) _ <-
            foldr (:) [] (dtx ^. bodyTxL . outputsTxBodyL)
        ] of
        datum : _ -> Just datum
        [] -> Nothing

-- | Decode a v2 cage request datum into the moog 'CageDatum'. The
-- offchain builder serialises it as
-- @Constr 0 [Constr 0 [tokenId, owner, key, value, fee, submittedAt]]@
-- (the @OnChainRequest@ shape), so strip the outer 'RequestDatum'
-- wrapper plus the @fee@\/@submittedAt@ fields the legacy four-field
-- datum lacked, then reuse the existing field decoders.
decodeRequestDatum
    :: (FromJSON Maybe k, FromData (Operation op))
    => Data
    -> Maybe (CageDatum k op)
decodeRequestDatum = \case
    Constr 0 [Constr 0 [tokenIdD, ownerD, keyD, valueD, _, _]] ->
        RequestDatum
            <$> fromData tokenIdD
            <*> fromData ownerD
            <*> (Change <$> fromData keyD <*> fromData valueD)
    _ -> Nothing

data Context = Context
    { mpfs :: Call
    , wait180S :: Wallet -> Submission ClientM
    , tokenId :: TokenId
    , requesterWallet :: Wallet
    , oracleWallet :: Wallet
    , agentWallet :: Wallet
    , auth :: Auth
    }

data RejectBoundaryProof = RejectBoundaryProof
    { rejectBoundaryRejectedRequest :: RequestRefId
    , rejectBoundaryRootBefore :: Root
    , rejectBoundaryRootAfter :: Root
    , rejectBoundaryPendingBefore :: [RequestRefId]
    , rejectBoundaryPendingAfter :: [RequestRefId]
    , rejectBoundaryProcessableRefused :: Bool
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
            $ BootToken oracleWallet canaryBootParams
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

-- | Poll @GET token@ until the parsed token satisfies the predicate,
-- giving the indexer time to surface a just-submitted request or a
-- just-applied update. Mirrors 'waitTx''s bounded retry.
pollToken
    :: Call
    -> TokenId
    -> (Token Identity -> Bool)
    -> IO (Token Identity)
pollToken (Call call) tk ok = go (180 :: Int)
  where
    go :: Int -> IO (Token Identity)
    go 0 = error "pollToken: token never reached the expected state"
    go n = do
        mToken <-
            (fromJSON <$> call (getToken tk))
                `catch` \(_ :: SomeException) -> pure Nothing
        case mToken of
            Just token | ok token -> pure token
            _ -> do
                threadDelay 1000000
                go (n - 1)

pendingRequestRefs :: Token Identity -> [RequestRefId]
pendingRequestRefs =
    fmap (requestZooRefId . runIdentity) . tokenRequests

submitBoundaryInsert :: Context -> JSValue -> JSValue -> IO RequestRefId
submitBoundaryInsert Context{mpfs, wait180S, tokenId, requesterWallet} key value = do
    WithTxHash txHash _ <-
        calling mpfs
            $ submit (wait180S requesterWallet)
            $ \address ->
                requestInsertFromFacts address tokenId
                    $ RequestInsertBody key value
    waitTx mpfs txHash
    pending <- pollToken mpfs tokenId $ not . null . tokenRequests
    case pendingRequestRefs pending of
        [reqId] -> pure reqId
        reqIds ->
            expectationFailure
                ( "expected one pending boundary request, got "
                    <> show reqIds
                )
                >> error "unreachable"

rejectBoundaryRequest
    :: Context
    -> RequestRefId
    -> IO (AValidationResult TokenRejectFailure TxHash)
rejectBoundaryRequest Context{mpfs, wait180S, tokenId, oracleWallet, auth} reqId =
    calling mpfs
        $ withContext mpfsClient (mkEffects auth) wait180S
        $ tokenCmdCore
        $ RejectToken tokenId oracleWallet [reqId]

rejectBoundaryRequestRefused :: Context -> RequestRefId -> IO Bool
rejectBoundaryRequestRefused ctx reqId =
    ( do
        result <- rejectBoundaryRequest ctx reqId
        case result of
            ValidationFailure _ -> pure True
            ValidationSuccess _ -> pure False
    )
        `catch` \(_ :: SomeException) -> pure True

rejectBoundaryProof :: Context -> IO RejectBoundaryProof
rejectBoundaryProof ctx@Context{mpfs, tokenId} = do
    _ <- pollToken mpfs tokenId $ null . tokenRequests
    rejectedRequest <-
        submitBoundaryInsert
            ctx
            (JSString "moog/reject-boundary/expired")
            (JSString "reject-me")

    -- canaryBootParams uses 5s process and 5s retract windows.
    threadDelay 12000000
    beforeReject <-
        pollToken mpfs tokenId
            $ (== [rejectedRequest]) . pendingRequestRefs
    let rejectBoundaryRootBefore = tokenRoot $ tokenState beforeReject
        rejectBoundaryPendingBefore = pendingRequestRefs beforeReject

    rejectResult <- rejectBoundaryRequest ctx rejectedRequest
    rejectTxHash <- case rejectResult of
        ValidationSuccess txHash -> pure txHash
        ValidationFailure err ->
            expectationFailure
                ("expired reject was refused: " <> show err)
                >> error "unreachable"
    waitTx mpfs rejectTxHash
    afterReject <-
        pollToken mpfs tokenId
            $ notElem rejectedRequest . pendingRequestRefs
    let rejectBoundaryRootAfter = tokenRoot $ tokenState afterReject
        rejectBoundaryPendingAfter = pendingRequestRefs afterReject

    freshRequest <-
        submitBoundaryInsert
            ctx
            (JSString "moog/reject-boundary/fresh")
            (JSString "reject-too-early")
    rejectBoundaryProcessableRefused <-
        rejectBoundaryRequestRefused ctx freshRequest
    when rejectBoundaryProcessableRefused $ do
        threadDelay 12000000
        cleanupResult <- rejectBoundaryRequest ctx freshRequest
        cleanupTxHash <- case cleanupResult of
            ValidationSuccess txHash -> pure txHash
            ValidationFailure err ->
                expectationFailure
                    ("cleanup reject was refused: " <> show err)
                    >> error "unreachable"
        waitTx mpfs cleanupTxHash
        _ <- pollToken mpfs tokenId $ null . tokenRequests
        pure ()

    pure
        RejectBoundaryProof
            { rejectBoundaryRejectedRequest = rejectedRequest
            , rejectBoundaryRootBefore
            , rejectBoundaryRootAfter
            , rejectBoundaryPendingBefore
            , rejectBoundaryPendingAfter
            , rejectBoundaryProcessableRefused
            }

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
        it "can retrieve a request-insert tx"
            $ \Context{mpfs = Call call, tokenId, requesterWallet} -> do
                WithUnsignedTx (UnsignedTx tx) _ <-
                    call
                        $ requestInsertFromFacts
                            requesterWallet.address
                            tokenId
                        $ RequestInsertBody
                            (JSString "key")
                            (JSString "value")
                let dtx = deserializeTx tx
                Just datum <- pure $ firstInlineDatum dtx
                Just (cageDatum :: CageDatum String (OpI String)) <-
                    pure $ decodeRequestDatum datum
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
                        $ requestDeleteFromFacts
                            requesterWallet.address
                            tokenId
                        $ RequestDeleteBody
                            (JSString "key")
                            (JSString "value")
                let dtx = deserializeTx tx
                Just datum <- pure $ firstInlineDatum dtx
                Just (cageDatum :: CageDatum String (OpD String)) <-
                    pure $ decodeRequestDatum datum
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
                        $ requestUpdateFromFacts
                            requesterWallet.address
                            tokenId
                        $ RequestUpdateBody
                            (JSString "key")
                            (JSString "oldValue")
                            (JSString "newValue")
                let dtx = deserializeTx tx
                Just datum <- pure $ firstInlineDatum dtx
                Just (cageDatum :: CageDatum String (OpU String String)) <-
                    pure $ decodeRequestDatum datum
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
        it "applies a config request through the full oracle cycle"
            $ \Context
                    { mpfs
                    , wait180S
                    , tokenId
                    , requesterWallet
                    , oracleWallet
                    , agentWallet
                    , auth
                    } -> do
                let config =
                        mkCurrentConfig agentWallet.owner
                            $ TestRunValidationConfig
                                { maxDuration = 3
                                , minDuration = 1
                                }
                -- 1. Requester submits a config-insert request. Config is
                -- the one typed request validateRequest accepts with no
                -- external GitHub state; it must come from the oracle,
                -- which the shared role wallets satisfy.
                _ <-
                    calling mpfs
                        $ withContext mpfsClient (mkEffects auth) wait180S
                        $ configCmd
                        $ SetConfig tokenId requesterWallet config
                -- 2. Oracle observes the single pending request.
                pending <-
                    pollToken mpfs tokenId
                        $ not . null . tokenRequests
                reqId <- case tokenRequests pending of
                    [Identity r@(InsertConfigRequest _)] ->
                        pure $ requestZooRefId r
                    rs ->
                        expectationFailure
                            ( "expected one pending config request, got "
                                <> show (runIdentity <$> rs)
                            )
                            >> error "unreachable"
                -- 3. Oracle validates (validateRequest, run inside
                -- UpdateToken) and applies via mpfsUpdateTokenFromFacts,
                -- signed by the oracle wallet. ValidationSuccess implies
                -- validation passed.
                applyResult <-
                    calling mpfs
                        $ withContext mpfsClient (mkEffects auth) wait180S
                        $ tokenCmdCore
                        $ UpdateToken tokenId oracleWallet [reqId]
                applyTxHash <- case applyResult of
                    ValidationSuccess h -> pure h
                    ValidationFailure e ->
                        expectationFailure
                            ( "oracle UpdateToken rejected the cycle: "
                                <> show e
                            )
                            >> error "unreachable"
                waitTx mpfs applyTxHash
                -- 4. Post-state: the request is consumed and the config
                -- fact the requester asked for is committed to the token.
                applied <-
                    pollToken mpfs tokenId
                        $ null . tokenRequests
                tokenRequests applied `shouldBe` []
                factsJson <- calling mpfs $ getTokenFacts tokenId
                map
                    factValue
                    (parseFacts factsJson :: [Fact ConfigKey Config])
                    `shouldBe` [config]
        it "rejects an expired request without changing the token root and refuses a fresh reject"
            $ \ctx@Context{} -> do
                RejectBoundaryProof{..} <- rejectBoundaryProof ctx
                rejectBoundaryPendingBefore
                    `shouldBe` [rejectBoundaryRejectedRequest]
                rejectBoundaryPendingAfter `shouldBe` []
                rejectBoundaryRootAfter `shouldBe` rejectBoundaryRootBefore
                rejectBoundaryProcessableRefused `shouldBe` True
