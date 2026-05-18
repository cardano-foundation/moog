{-# LANGUAGE NumericUnderscores #-}

module MPFS.Canary
    ( BootCanaryFailure (..)
    , BootCanaryResult (..)
    , bootCanary
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
    ( TxHash
    , WithTxHash (..)
    )
import Core.Types.Wallet (Wallet)
import Lib.JSON.Canonical.Extra
    ( boolJSON
    , intJSON
    , object
    , (.=)
    )
import MPFS.API
    ( MPFS (..)
    , mpfsClient
    )
import Submitting (Submission (..))
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , JSValue
    , ToJSON (..)
    )

data BootCanaryFailure
    = NoBootTokenIdReturned
    | BootTokenIdNotValidJSON
    | BoundaryNotObserved String String
    deriving (Show, Eq)

instance Exception BootCanaryFailure

data BootCanaryResult = BootCanaryResult
    { canaryTxHash :: TxHash
    , canaryTokenId :: TokenId
    , transactionPolls :: Int
    , tokenPolls :: Int
    }
    deriving (Show, Eq)

instance Monad m => ToJSON m BootCanaryResult where
    toJSON BootCanaryResult{..} =
        object
            [ "boundary" .= ("mpfs-v2-boot" :: String)
            , "txHash" .= canaryTxHash
            , "tokenId" .= canaryTokenId
            , ("transactionObserved", boolJSON True)
            , ("tokenObserved", boolJSON True)
            , ("transactionPolls", intJSON transactionPolls)
            , ("tokenPolls", intJSON tokenPolls)
            ]

bootCanary
    :: MPFSClient
    -> Wallet
    -> Int
    -> IO BootCanaryResult
bootCanary MPFSClient{runMPFS, submitTx} wallet maxPolls = do
    WithTxHash txHash rawTokenId <- runMPFS $ do
        let Submission submit = submitTx wallet
        submit $ mpfsBootToken mpfsClient
    tokenId <-
        either throwIO pure $ tokenIdFromBootValue rawTokenId
    transactionPolls <-
        pollBoundary "transaction" maxPolls
            $ runMPFS
            $ mpfsGetTransaction mpfsClient txHash
    tokenPolls <-
        pollBoundary "token" maxPolls
            $ runMPFS
            $ mpfsGetToken mpfsClient tokenId
    pure
        BootCanaryResult
            { canaryTxHash = txHash
            , canaryTokenId = tokenId
            , transactionPolls
            , tokenPolls
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
pollBoundary boundary maxPolls action =
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
                    threadDelay 1_000_000
                    go (polls + 1)
