module Wallet.CliSpec
    ( spec
    )
where

import Core.Types.Mnemonics
    ( Mnemonics (..)
    )
import Core.Types.Wallet (Wallet)
import Data.Either (fromRight, isLeft, isRight)
import Data.Text (Text)
import Submitting (readWallet)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import Wallet.Cli
    ( WalletCommand (Create, Decrypt, Encrypt, Info)
    , WalletError (WalletPresent, WalletAlreadyDecrypted)
    , WalletInfo (..)
    , walletCmd
    )

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Submitting as Submitting

tryObtainingDecryptedWallet :: Text -> Either Submitting.WalletError Wallet
tryObtainingDecryptedWallet mnemonicText =
    let mnemonic = ClearText mnemonicText
    in readWallet (False, mnemonic)

tryRetrieveCreatedWallet :: FilePath -> IO (Either Submitting.WalletError Wallet)
tryRetrieveCreatedWallet filepath = do
    mnemonicObj <- T.readFile filepath
    let mnemonicText =
            fst . T.breakOn "\"}" .
            snd . T.breakOnEnd ":\"" . T.strip $ mnemonicObj
    pure $ tryObtainingDecryptedWallet mnemonicText

getDecryptedWalletForTesting :: Wallet
getDecryptedWalletForTesting = do
    let mnemonicText = "culture island clump online fatigue curve fish during mandate echo cradle cat arrange upset region"
        decryptedWalE = tryObtainingDecryptedWallet mnemonicText
    fromRight (error "after above check wallet is sure to be properly formed") decryptedWalE

spec :: Spec
spec = do
    describe "wallet create" $ do
        it "fails if the wallet is already present" $ do
            withSystemTempDirectory "wallet-cli-spec" $ \dir -> do
                let wallet = dir <> "/wallet"
                writeFile wallet ""
                let command = Create wallet (Nothing :: Maybe Text)
                walletCmd command `shouldReturn` Left WalletPresent
        it "succeeds if the wallet is not present" $ do
            withSystemTempDirectory "wallet-cli-spec" $ \dir -> do
                let wallet = dir <> "/wallet"
                let command = Create wallet (Nothing :: Maybe Text)
                res <- walletCmd command
                res `shouldSatisfy` isRight
    describe "wallet encrypt/decrypt" $ do
        it "wallet can be created from valid mnemonic" $ do
            let mnemonicText = "culture island clump online fatigue curve fish during mandate echo cradle cat arrange upset region"
                decryptedWal = tryObtainingDecryptedWallet mnemonicText
            decryptedWal `shouldSatisfy` isRight
        it "wallet cannot be created from invalid mnemonic - wrong number of words" $ do
            let mnemonicText = "culture island clump online fatigue curve fish mandate echo cradle cat arrange upset region"
                decryptedWal = tryObtainingDecryptedWallet mnemonicText
            decryptedWal `shouldSatisfy` isLeft
        it "wallet cannot be created from invalid mnemonic - proper number of words, not all legal" $ do
            let mnemonicText = "culture island clump online fatigue curve fish mandate echo cradle cat arrange upset regions"
                decryptedWal = tryObtainingDecryptedWallet mnemonicText
            decryptedWal `shouldSatisfy` isLeft
        it "wallet can be encrypted when in decrypted state" $ do
            withSystemTempDirectory "wallet-cli-spec" $ \dir -> do
                let walletDir = dir <> "/wallet"
                let decryptedWal = getDecryptedWalletForTesting
                let commandEnc = Encrypt decryptedWal "password" walletDir
                res <- walletCmd commandEnc
                res `shouldSatisfy` isRight
        it "wallet cannot be decrypted when in decrypted state" $ do
            withSystemTempDirectory "wallet-cli-spec" $ \dir -> do
                let walletDir = dir <> "/wallet"
                let decryptedWal = getDecryptedWalletForTesting
                let commandDecr = Decrypt decryptedWal walletDir
                walletCmd commandDecr `shouldReturn` Left WalletAlreadyDecrypted
        it "wallet cannot be decrypted when created previously in decrypted state" $ do
            withSystemTempDirectory "wallet-cli-spec" $ \dir -> do
                let walletDir = dir <> "/wallet"
                    commandCreate = Create walletDir (Nothing :: Maybe Text)
                res <- walletCmd commandCreate
                res `shouldSatisfy` isRight
                decryptedWalE <- tryRetrieveCreatedWallet walletDir
                decryptedWalE `shouldSatisfy` isRight
                let decryptedWal = fromRight (error "after above check wallet is sure to be properly formed") decryptedWalE
                let commandDecr = Decrypt decryptedWal walletDir
                walletCmd commandDecr `shouldReturn` Left WalletAlreadyDecrypted
                let commandInfo = Info decryptedWal
                infoRes <- walletCmd commandInfo
                encryptedInfo <$> infoRes `shouldBe` Right False
        it "wallet can be encrypted when created previously in decrypted state" $ do
            withSystemTempDirectory "wallet-cli-spec" $ \dir -> do
                let walletDir = dir <> "/wallet"
                    commandCreate = Create walletDir (Nothing :: Maybe Text)
                res <- walletCmd commandCreate
                res `shouldSatisfy` isRight
                decryptedWalE <- tryRetrieveCreatedWallet walletDir
                decryptedWalE `shouldSatisfy` isRight
                let decryptedWal = fromRight (error "after above check wallet is sure to be properly formed") decryptedWalE
                let commandEnc = Encrypt decryptedWal "password" walletDir
                walletCmd commandEnc `shouldReturn` Left WalletPresent
