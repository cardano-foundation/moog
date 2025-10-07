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
import Test.Hspec (Spec, describe, it, shouldReturn, shouldSatisfy)
import Wallet.Cli
    ( WalletCommand (Create, Decrypt,Encrypt)
    , WalletError (WalletPresent, WalletAlreadyDecrypted)
    , walletCmd
    )

import qualified Submitting as Submitting

tryObtainingDecryptedWallet :: Text -> Either Submitting.WalletError Wallet
tryObtainingDecryptedWallet mnemonicText =
    let mnemonic = ClearText mnemonicText
    in readWallet (False, mnemonic)

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
