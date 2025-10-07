{-# LANGUAGE OverloadedRecordDot #-}

module Wallet.Cli
    ( walletCmd
    , WalletCommand (..)
    , WalletError (..)
    , WalletInfo (..)
    ) where

import Control.Monad (replicateM)
import Core.Types.Basic (Address, Owner)
import Core.Types.Mnemonics
    ( Mnemonics (..)
    )
import Core.Types.VKey (VKey (..), encodeVKey)
import Core.Types.Wallet (Wallet (..))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Lib.JSON.Canonical.Extra (object, (.=))
import Submitting (walletFromMnemonic, writeWallet)
import System.Random (randomRIO)
import Text.JSON.Canonical
    ( JSValue (JSString)
    , ToJSON (..)
    )
import Words (englishWords)

data WalletError
    = WalletPresent
    | WalletAlreadyDecrypted
    | WalletAlreadyEncrypted
    deriving (Show, Eq)

instance Applicative m => ToJSON m WalletError where
    toJSON WalletPresent = pure $ JSString "Wallet is present"
    toJSON WalletAlreadyDecrypted = pure $ JSString "Wallet's file is already decrypted"
    toJSON WalletAlreadyEncrypted = pure $ JSString "Wallet's file is already encrypted"

instance (ToJSON m a, Monad m) => ToJSON m (Either WalletError a) where
    toJSON (Right a) = toJSON a
    toJSON (Left e) = object ["error" .= e]

data WalletInfo = WalletInfo
    { address :: Address
    , owner :: Owner
    , encryptedInfo :: Bool
    , publicKey :: String -- bech32-encoded public key
    }
    deriving (Show, Eq)

instance Monad m => ToJSON m WalletInfo where
    toJSON WalletInfo{address, owner, encryptedInfo, publicKey} =
        object
            [ "address" .= address
            , "owner" .= owner
            , "encrypted"
                .= if encryptedInfo
                    then JSString "yes"
                    else JSString "no"
            , "publicKey" .= publicKey
            ]

data WalletCommand a where
    Info
        :: Wallet -> WalletCommand (Either WalletError WalletInfo)
    Create
        :: FilePath
        -> Maybe Text
        -> WalletCommand (Either WalletError WalletInfo)
    Decrypt
        :: Wallet
        -> FilePath
        -> WalletCommand (Either WalletError WalletInfo)
    Encrypt
        :: Wallet
        -> Text -- passphrase
        -> FilePath
        -> WalletCommand (Either WalletError WalletInfo)

deriving instance Show (WalletCommand a)
deriving instance Eq (WalletCommand a)

failIfWalletExists :: IO Bool -> a -> IO (Either WalletError a)
failIfWalletExists check result = do
    exists <- check
    if exists
        then return $ Left WalletPresent
        else return $ Right result

mkWalletVKey :: Wallet -> String
mkWalletVKey Wallet{privateKey} = case encodeVKey $ Ed25519.toPublic privateKey of
    Left err -> error $ "Failed to encode wallet public key: " ++ show err
    Right (VKey vkey) -> T.unpack vkey

walletCmd :: WalletCommand a -> IO a
walletCmd (Info wallet) =
    pure
        $ Right
        $ WalletInfo
            { address = wallet.address
            , owner = wallet.owner
            , encryptedInfo = wallet.encrypted
            , publicKey = mkWalletVKey wallet
            }
walletCmd (Create walletFile passphrase) = do
    w12 <- replicateM 12 $ element englishWords
    let mnemonics = ClearText $ T.unwords w12
    case walletFromMnemonic True mnemonics of
        Left _e -> walletCmd (Create walletFile passphrase)
        Right wallet -> do
            failIfWalletExists (writeWallet walletFile mnemonics passphrase)
                $ WalletInfo
                    { address = wallet.address
                    , owner = wallet.owner
                    , encryptedInfo = isJust passphrase
                    , publicKey = mkWalletVKey wallet
                    }
walletCmd (Decrypt wallet walletFileDecr) =
    if encrypted wallet
        then do
            failIfWalletExists
                (writeWallet walletFileDecr (mnemonics wallet) Nothing)
                $ WalletInfo
                    { address = wallet.address
                    , owner = wallet.owner
                    , encryptedInfo = False
                    , publicKey = mkWalletVKey wallet
                    }
        else
            pure $ Left WalletAlreadyDecrypted
walletCmd (Encrypt wallet passphrase walletFileDecr) =
    if encrypted wallet
        then do
            pure $ Left WalletAlreadyEncrypted
        else do
            failIfWalletExists
                (writeWallet walletFileDecr (mnemonics wallet) (Just passphrase))
                $ WalletInfo
                    { address = wallet.address
                    , owner = wallet.owner
                    , encryptedInfo = True
                    , publicKey = mkWalletVKey wallet
                    }

element :: [a] -> IO a
element xs = do
    idx <- randomRIO (0, length xs - 1)
    return $ xs !! idx
