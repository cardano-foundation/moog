{-# LANGUAGE NumericUnderscores #-}

module MPFS.Canary
    ( BootCanaryFailure (..)
    , FullLifecycleCanaryResult (..)
    , fullLifecycleCanary
    , pollGoneBoundaryWithDelay
    , tokenIdFromBootValue
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception
    ( Exception
    , SomeException
    , throwIO
    , try
    )
import Control.Monad (unless)
import Core.Types.Basic
    ( RequestRefId
    , TokenId
    )
import Core.Types.Fact
    ( Fact (..)
    , Slot (..)
    , parseFacts
    )
import Core.Types.MPFS (MPFSClient (..))
import Core.Types.Tx
    ( TxHash (..)
    , WithUnsignedTx (..)
    )
import Core.Types.Wallet (Wallet (..))
import Data.Functor.Identity (Identity (..))
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)
import Lib.JSON.Canonical.Extra
    ( boolJSON
    , intJSON
    , object
    , (.=)
    )
import MPFS.API
    ( BootParams (..)
    , MPFS (..)
    , RequestInsertBody (..)
    , awaitTransactionV2
    , getTokenV2
    , mpfsClient
    , submitTransactionV2
    )
import Oracle.Types
    ( Token (..)
    , requestZooRefId
    )
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , JSValue (..)
    , ToJSON (..)
    , toJSString
    )

data BootCanaryFailure
    = NoBootTokenIdReturned
    | BootTokenIdNotValidJSON
    | BoundaryNotObserved String String
    | BoundaryStillObserved String
    deriving (Show, Eq)

instance Exception BootCanaryFailure

data FullLifecycleCanaryResult = FullLifecycleCanaryResult
    { canaryBootTxHash :: TxHash
    , canaryRequestInsertTxHash :: TxHash
    , canaryUpdateTokenTxHash :: TxHash
    , canaryEndTxHash :: TxHash
    , canaryTokenId :: TokenId
    , bootTransactionPolls :: Int
    , requestInsertTransactionPolls :: Int
    , requestObservedPolls :: Int
    , updateTokenTransactionPolls :: Int
    , factObservedPolls :: Int
    , tokenBeforeEndPolls :: Int
    , endTransactionPolls :: Int
    , tokenGonePolls :: Int
    }
    deriving (Show, Eq)

instance Monad m => ToJSON m FullLifecycleCanaryResult where
    toJSON FullLifecycleCanaryResult{..} =
        object
            [ "boundary" .= ("mpfs-v2-full-lifecycle" :: String)
            , "bootTxHash" .= textOf canaryBootTxHash
            , "requestInsertTxHash" .= textOf canaryRequestInsertTxHash
            , "updateTokenTxHash" .= textOf canaryUpdateTokenTxHash
            , "endTxHash" .= textOf canaryEndTxHash
            , "tokenId" .= canaryTokenId
            , ("bootTransactionObserved", boolJSON True)
            , ("requestInsertTransactionObserved", boolJSON True)
            , ("requestObserved", boolJSON True)
            , ("updateTokenTransactionObserved", boolJSON True)
            , ("factObserved", boolJSON True)
            , ("endTransactionObserved", boolJSON True)
            , ("tokenObservedBeforeEnd", boolJSON True)
            , ("tokenGoneObserved", boolJSON True)
            , ("bootTransactionPolls", intJSON bootTransactionPolls)
            ,
                ( "requestInsertTransactionPolls"
                , intJSON requestInsertTransactionPolls
                )
            , ("requestObservedPolls", intJSON requestObservedPolls)
            ,
                ( "updateTokenTransactionPolls"
                , intJSON updateTokenTransactionPolls
                )
            , ("factObservedPolls", intJSON factObservedPolls)
            , ("tokenBeforeEndPolls", intJSON tokenBeforeEndPolls)
            , ("endTransactionPolls", intJSON endTransactionPolls)
            , ("tokenGonePolls", intJSON tokenGonePolls)
            ]

-- | Devnet boot economics for the canary: short 5s windows are fine
-- on a fast local devnet, where the oracle can fold within seconds.
canaryBootParams :: BootParams
canaryBootParams =
    BootParams
        { bootProcessTimeMs = 5_000
        , bootRetractTimeMs = 5_000
        , bootTipLovelace = 1_000_000
        }

fullLifecycleCanary
    :: MPFSClient
    -> Wallet
    -> Int
    -> IO FullLifecycleCanaryResult
fullLifecycleCanary MPFSClient{runMPFS} wallet@Wallet{sign} maxPolls = do
    WithUnsignedTx unsignedTx rawTokenId <-
        runMPFS $ mpfsBootToken mpfsClient canaryBootParams (address wallet)
    signedTx <-
        case sign unsignedTx of
            Right tx -> pure tx
            Left err -> throwIO err
    bootTxHash <-
        runMPFS $ submitTransactionV2 signedTx
    tokenId <-
        either throwIO pure $ tokenIdFromBootValue rawTokenId
    bootTransactionPolls <-
        pollBoundary "transaction" maxPolls
            $ runMPFS
            $ awaitTransactionV2 bootTxHash
    tokenBeforeEndPolls <-
        pollBoundary "token before end" maxPolls
            $ runMPFS
            $ getTokenV2 tokenId
    WithUnsignedTx requestUnsignedTx _ <-
        runMPFS
            $ mpfsRequestInsertFromFacts
                mpfsClient
                (address wallet)
                tokenId
            $ RequestInsertBody canaryFactKey canaryFactValue
    signedRequestTx <-
        case sign requestUnsignedTx of
            Right tx -> pure tx
            Left err -> throwIO err
    requestInsertTxHash <-
        runMPFS $ submitTransactionV2 signedRequestTx
    requestInsertTransactionPolls <-
        pollBoundary "request insert transaction" maxPolls
            $ runMPFS
            $ awaitTransactionV2 requestInsertTxHash
    (requestRefId, requestObservedPolls) <-
        pollObservedBoundaryValue "request" maxPolls
            $ observedRequestRefId
            =<< runMPFS (mpfsGetToken mpfsClient tokenId)
    WithUnsignedTx updateUnsignedTx _ <-
        runMPFS
            $ mpfsUpdateTokenFromFacts
                mpfsClient
                (address wallet)
                tokenId
                [requestRefId]
    signedUpdateTx <-
        case sign updateUnsignedTx of
            Right tx -> pure tx
            Left err -> throwIO err
    updateTokenTxHash <-
        runMPFS $ submitTransactionV2 signedUpdateTx
    updateTokenTransactionPolls <-
        pollBoundary "update token transaction" maxPolls
            $ runMPFS
            $ awaitTransactionV2 updateTokenTxHash
    factObservedPolls <-
        pollBoundary "fact" maxPolls
            $ assertCanaryFact
            =<< runMPFS (mpfsGetTokenFacts mpfsClient tokenId)
    WithUnsignedTx endUnsignedTx endValue <-
        runMPFS $ mpfsEndToken mpfsClient (address wallet) tokenId
    case endValue of
        Nothing -> pure ()
        Just _ ->
            throwIO
                $ userError
                    "MPFS v2 end facts path returned a token value"
    signedEndTx <-
        case sign endUnsignedTx of
            Right tx -> pure tx
            Left err -> throwIO err
    endTxHash <-
        runMPFS $ submitTransactionV2 signedEndTx
    endTransactionPolls <-
        pollBoundary "end transaction" maxPolls
            $ runMPFS
            $ awaitTransactionV2 endTxHash
    tokenGonePolls <-
        pollGoneBoundary "token gone" maxPolls
            $ runMPFS
            $ getTokenV2 tokenId
    pure
        FullLifecycleCanaryResult
            { canaryBootTxHash = bootTxHash
            , canaryRequestInsertTxHash = requestInsertTxHash
            , canaryUpdateTokenTxHash = updateTokenTxHash
            , canaryEndTxHash = endTxHash
            , canaryTokenId = tokenId
            , bootTransactionPolls
            , requestInsertTransactionPolls
            , requestObservedPolls
            , updateTokenTransactionPolls
            , factObservedPolls
            , tokenBeforeEndPolls
            , endTransactionPolls
            , tokenGonePolls
            }

canaryFactKey :: JSValue
canaryFactKey =
    JSString $ toJSString "moog-mpfs-v2-canary/full-lifecycle"

canaryFactValue :: JSValue
canaryFactValue =
    JSString $ toJSString "write-submit-observed"

observedRequestRefId :: JSValue -> IO RequestRefId
observedRequestRefId value =
    case fromJSON value of
        Just token ->
            maybe
                (throwIO $ userError "verified token read has no requests")
                pure
                $ firstRequestRefId token
        Nothing ->
            throwIO $ userError "verified token read did not parse"

firstRequestRefId :: Token Identity -> Maybe RequestRefId
firstRequestRefId Token{tokenRequests} =
    requestZooRefId . runIdentity <$> listToMaybe tokenRequests

assertCanaryFact :: JSValue -> IO ()
assertCanaryFact value =
    unless factObserved
        $ throwIO
        $ userError
            "verified facts read did not include the canary insert"
  where
    facts = parseFacts value
    factObserved =
        any
            ( \(Fact key value' (Slot _)) ->
                key == canaryFactKey && value' == canaryFactValue
            )
            facts

tokenIdFromBootValue
    :: Maybe JSValue -> Either BootCanaryFailure TokenId
tokenIdFromBootValue Nothing =
    Left NoBootTokenIdReturned
tokenIdFromBootValue (Just value) =
    case fromJSON value of
        Just tokenId -> Right tokenId
        Nothing -> Left BootTokenIdNotValidJSON

pollBoundary :: String -> Int -> IO a -> IO Int
pollBoundary boundary maxPolls =
    fmap snd
        . pollObservedBoundaryValueWithDelay
            boundary
            maxPolls
            (threadDelay 1_000_000)

pollObservedBoundaryValue
    :: String -> Int -> IO a -> IO (a, Int)
pollObservedBoundaryValue boundary maxPolls =
    pollObservedBoundaryValueWithDelay
        boundary
        maxPolls
        (threadDelay 1_000_000)

pollObservedBoundaryValueWithDelay
    :: String -> Int -> IO () -> IO a -> IO (a, Int)
pollObservedBoundaryValueWithDelay boundary maxPolls delay action =
    go 0
  where
    go polls = do
        result <- try action
        case result of
            Right value -> pure (value, polls)
            Left (err :: SomeException)
                | polls >= maxPolls ->
                    throwIO
                        $ BoundaryNotObserved boundary
                        $ show err
                | otherwise -> do
                    delay
                    go (polls + 1)

pollGoneBoundary :: String -> Int -> IO a -> IO Int
pollGoneBoundary boundary maxPolls =
    pollGoneBoundaryWithDelay
        boundary
        maxPolls
        (threadDelay 1_000_000)

pollGoneBoundaryWithDelay :: String -> Int -> IO () -> IO a -> IO Int
pollGoneBoundaryWithDelay boundary maxPolls delay action =
    go 0
  where
    go polls = do
        result <- try action
        case result of
            Left (err :: SomeException)
                | isTokenGoneError err -> pure polls
                | polls >= maxPolls ->
                    throwIO
                        $ BoundaryNotObserved boundary
                        $ show err
                | otherwise -> do
                    delay
                    go (polls + 1)
            Right _
                | polls >= maxPolls ->
                    throwIO $ BoundaryStillObserved boundary
                | otherwise -> do
                    delay
                    go (polls + 1)

isTokenGoneError :: SomeException -> Bool
isTokenGoneError err =
    any (`isInfixOf` shown) ["404", "Not Found", "not found"]
  where
    shown = show err
