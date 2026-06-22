module Core.WalletVaultSpec
    ( spec
    )
where

import Cardano.Tx.Sign.Vault.Age
    ( AgeVaultError (AgeVaultDecryptFailure)
    )
import Core.WalletVault
    ( decryptWalletVaultText
    , encryptWalletVaultText
    )
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

mnemonics :: Text
mnemonics =
    "culture island clump online fatigue curve fish during mandate echo cradle cat arrange upset region"

passphrase :: Text
passphrase = "correct horse battery staple"

spec :: Spec
spec =
    describe "Core.WalletVault" $ do
        it "round-trips mnemonics through an age wallet vault" $ do
            encrypted <- encryptWalletVaultText passphrase mnemonics
            encrypted `shouldSatisfy` T.isPrefixOf "age-v1:"
            decryptWalletVaultText passphrase encrypted `shouldBe` Right mnemonics

        it "rejects a wrong passphrase" $ do
            encrypted <- encryptWalletVaultText passphrase mnemonics
            decryptWalletVaultText "wrong passphrase" encrypted
                `shouldBe` Left AgeVaultDecryptFailure

        it "rejects a tampered vault" $ do
            encrypted <- encryptWalletVaultText passphrase mnemonics
            let tampered =
                    case T.unsnoc encrypted of
                        Nothing -> encrypted
                        Just (prefix, lastChar) ->
                            prefix
                                <> T.singleton
                                    (if lastChar == 'A' then 'B' else 'A')
            decryptWalletVaultText passphrase tampered
                `shouldBe` Left AgeVaultDecryptFailure
