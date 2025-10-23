module Core.Types.Mnemonics.Options
    ( mnemonicsParser
    , walletPassphraseCommon
    , walletFileOption
    , walletFileArgOption
    ) where

import Control.Exception (try)
import Core.Encryption (decryptText)
import Core.Types.Mnemonics (Mnemonics (..), MnemonicsPhase (..))
import Data.Aeson
    ( Object
    )
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Lib.Options.Secrets (secretsParser)
import OptEnvConf
    ( Alternative ((<|>))
    , Parser
    , argument
    , checkEither
    , checkMapIO
    , conf
    , env
    , help
    , long
    , metavar
    , option
    , reader
    , setting
    , short
    , str
    , withConfig
    )

mnemonicsClearTextOption :: Parser Text
mnemonicsClearTextOption =
    setting
        [ help "The mnemonics for the wallet in clear text"
        , conf "mnemonics"
        , metavar "MNEMONICS"
        ]

mnemonicsEncryptedOption :: Parser Text
mnemonicsEncryptedOption =
    setting
        [ help "The encrypted mnemonics for the wallet"
        , conf "encryptedMnemonics"
        , metavar "ENCRYPTED_MNEMONICS"
        ]

walletPassphraseCommon :: Parser Text
walletPassphraseCommon =
    T.pack
        <$> secretsParser
            "Enter the passphrase to decrypt the mnemonics"
            "The passphrase to decrypt the mnemonics"
            "MOOG_WALLET_PASSPHRASE"
            "PASSPHRASE"
            "ask-wallet-passphrase"
            "walletPassphrase"

walletFileOption :: Parser FilePath
walletFileOption =
    setting
        [ env "MOOG_WALLET_FILE"
        , metavar "FILEPATH"
        , help "The file path to the wallet secret mnemonics"
        , long "wallet"
        , short 'w'
        , reader str
        , option
        ]

walletFileArgOption :: Parser FilePath
walletFileArgOption =
    setting
        [ help "The new file path to the wallet secret mnemonics"
        , metavar "FILEPATH"
        , reader str
        , argument
        ]

coreMnemonicsParser :: Parser (Bool, Mnemonics 'DecryptedS)
coreMnemonicsParser =
    (False,) . ClearText <$> mnemonicsClearTextOption
        <|> (True,) . ClearText
            <$> checkEither
                id
                (decryptText <$> walletPassphraseCommon <*> mnemonicsEncryptedOption)

readJSONFile :: FilePath -> IO (Either String Object)
readJSONFile fp = do
    econtent <- try $ BL.readFile fp
    case econtent of
        Left (e :: IOError) -> return $ Left $ show e
        Right content -> return $ Aeson.eitherDecode content

mnemonicsObject :: Parser Object
mnemonicsObject = checkMapIO readJSONFile walletFileOption

mnemonicsParser :: Parser (Bool, Mnemonics 'DecryptedS)
mnemonicsParser = withConfig (Just <$> mnemonicsObject) coreMnemonicsParser
