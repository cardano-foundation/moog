{-# LANGUAGE NumericUnderscores #-}

module MPFS.Canary
    ( BootCanaryFailure (..)
    , EndBoundaryCanaryResult (..)
    , bootThenEndCanary
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
import Core.Types.Basic (TokenId)
import Core.Types.MPFS (MPFSClient (..))
import Core.Types.Tx
    ( TxHash (..)
    , WithUnsignedTx (..)
    )
import Core.Types.Wallet (Wallet (..))
import Data.List (isInfixOf)
import Lib.JSON.Canonical.Extra
    ( boolJSON
    , intJSON
    , object
    , (.=)
    )
import MPFS.API
    ( MPFS (..)
    , awaitTransactionV2
    , getTokenV2
    , mpfsClient
    , submitTransactionV2
    )
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , JSValue
    , ToJSON (..)
    )

data BootCanaryFailure
    = NoBootTokenIdReturned
    | BootTokenIdNotValidJSON
    | BoundaryNotObserved String String
    | BoundaryStillObserved String
    deriving (Show, Eq)

instance Exception BootCanaryFailure

data EndBoundaryCanaryResult = EndBoundaryCanaryResult
    { canaryBootTxHash :: TxHash
    , canaryEndTxHash :: TxHash
    , canaryTokenId :: TokenId
    , bootTransactionPolls :: Int
    , tokenBeforeEndPolls :: Int
    , endTransactionPolls :: Int
    , tokenGonePolls :: Int
    }
    deriving (Show, Eq)

instance Monad m => ToJSON m EndBoundaryCanaryResult where
    toJSON EndBoundaryCanaryResult{..} =
        object
            [ "boundary" .= ("mpfs-v2-end" :: String)
            , "bootTxHash" .= textOf canaryBootTxHash
            , "endTxHash" .= textOf canaryEndTxHash
            , "tokenId" .= canaryTokenId
            , ("bootTransactionObserved", boolJSON True)
            , ("endTransactionObserved", boolJSON True)
            , ("tokenObservedBeforeEnd", boolJSON True)
            , ("tokenGoneObserved", boolJSON True)
            , ("bootTransactionPolls", intJSON bootTransactionPolls)
            , ("tokenBeforeEndPolls", intJSON tokenBeforeEndPolls)
            , ("endTransactionPolls", intJSON endTransactionPolls)
            , ("tokenGonePolls", intJSON tokenGonePolls)
            ]

bootThenEndCanary
    :: MPFSClient
    -> Wallet
    -> Int
    -> IO EndBoundaryCanaryResult
bootThenEndCanary MPFSClient{runMPFS} wallet@Wallet{sign} maxPolls = do
    WithUnsignedTx unsignedTx rawTokenId <-
        runMPFS $ mpfsBootToken mpfsClient (address wallet)
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
        EndBoundaryCanaryResult
            { canaryBootTxHash = bootTxHash
            , canaryEndTxHash = endTxHash
            , canaryTokenId = tokenId
            , bootTransactionPolls
            , tokenBeforeEndPolls
            , endTransactionPolls
            , tokenGonePolls
            }

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
    pollObservedBoundaryWithDelay
        boundary
        maxPolls
        (threadDelay 1_000_000)

pollObservedBoundaryWithDelay
    :: String -> Int -> IO () -> IO a -> IO Int
pollObservedBoundaryWithDelay boundary maxPolls delay action =
    go 0
  where
    go polls = do
        result <- try action
        case result of
            Right _ -> pure polls
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
