{-# LANGUAGE OverloadedRecordDot #-}

module Wallet.Cli
    ( walletCmd
    , WalletCommand (..)
    ) where

import Control.Monad (replicateM)
import Core.Types
    ( Address
    , Owner
    , Wallet (..)
    )
import Lib.JSON (object, (.=))
import Submitting (walletFromMnemonic, writeWallet)
import System.Random (randomRIO)
import Text.JSON.Canonical (JSValue (JSString), ToJSON (..))
import Words (englishWords)

data WalletError
    = WalletPresent
    | WalletMissing

instance Applicative m => ToJSON m WalletError where
    toJSON WalletPresent = pure $ JSString "Wallet is present"
    toJSON WalletMissing = pure $ JSString "Wallet is missing"

instance (ToJSON m a, Monad m) => ToJSON m (Either WalletError a) where
    toJSON (Right a) = toJSON a
    toJSON (Left e) = object ["error" .= e]

data WalletInfo = WalletInfo
    { address :: Address
    , owner :: Owner
    }

instance Monad m => ToJSON m WalletInfo where
    toJSON WalletInfo{address, owner} =
        object
            [ "address" .= address
            , "owner" .= owner
            ]

data WalletCommand a where
    Info :: WalletCommand (Either WalletError WalletInfo)
    Create :: WalletCommand (Either WalletError WalletInfo)

deriving instance Show (WalletCommand a)
deriving instance Eq (WalletCommand a)

walletCmd :: Either FilePath Wallet -> WalletCommand a -> IO a
walletCmd (Right wallet) Info =
    pure
        $ Right
        $ WalletInfo
            { address = wallet.address
            , owner = wallet.owner
            }
walletCmd (Left walletFile) Create = do
    w12 <- replicateM 12 $ element englishWords
    case walletFromMnemonic w12 of
        Left _e -> walletCmd (Left walletFile) Create
        Right wallet -> do
            writeWallet walletFile w12
            return
                $ Right
                $ WalletInfo
                    { address = wallet.address
                    , owner = wallet.owner
                    }
walletCmd (Left _) Info = pure $ Left WalletMissing
walletCmd (Right _) Create = pure $ Left WalletPresent

element :: [a] -> IO a
element xs = do
    idx <- randomRIO (0, length xs - 1)
    return $ xs !! idx
