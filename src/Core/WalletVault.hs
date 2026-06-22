module Core.WalletVault
    ( decryptWalletVaultText
    , encryptWalletVaultText
    , walletVaultPrefix
    ) where

import Cardano.Tx.Sign.Vault.Age
    ( AgeVaultError (AgeVaultDecryptFailure)
    , decryptAgeVault
    , defaultVaultWorkFactor
    , encryptAgeVault
    , mkVaultPassphrase
    , parseVaultWorkFactor
    , renderAgeVaultError
    )
import Data.ByteString.Base64 qualified as Base64
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

walletVaultPrefix :: Text
walletVaultPrefix = "age-v1:"

encryptWalletVaultText
    :: Text
    -> Text
    -> IO Text
encryptWalletVaultText passphrase plaintext = do
    vaultPassphrase <-
        unsafeAgeResult $ mkVaultPassphrase $ T.encodeUtf8 passphrase
    workFactor <-
        unsafeAgeResult $ parseVaultWorkFactor defaultVaultWorkFactor
    ciphertext <-
        unsafeAgeResult
            =<< encryptAgeVault
                workFactor
                vaultPassphrase
                (T.encodeUtf8 plaintext)
    pure
        $ walletVaultPrefix
            <> T.decodeUtf8 (Base64.encode ciphertext)

decryptWalletVaultText
    :: Text
    -> Text
    -> Either AgeVaultError Text
decryptWalletVaultText passphrase encoded = do
    payload <-
        maybe (Left AgeVaultDecryptFailure) Right
            $ T.stripPrefix walletVaultPrefix encoded
    ciphertext <-
        case Base64.decode $ T.encodeUtf8 payload of
            Left _ -> Left AgeVaultDecryptFailure
            Right decoded -> Right decoded
    vaultPassphrase <- mkVaultPassphrase $ T.encodeUtf8 passphrase
    workFactor <- parseVaultWorkFactor defaultVaultWorkFactor
    plaintext <- decryptAgeVault workFactor vaultPassphrase ciphertext
    case T.decodeUtf8' plaintext of
        Left _ -> Left AgeVaultDecryptFailure
        Right decoded -> Right decoded

unsafeAgeResult :: Either AgeVaultError a -> IO a
unsafeAgeResult =
    either (fail . T.unpack . renderAgeVaultError) pure
