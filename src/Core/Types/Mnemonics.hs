module Core.Types.Mnemonics
    ( Mnemonics (..)
    , MnemonicsPhase (..)
    ) where

import Data.Aeson
    ( ToJSON (..)
    , object
    , (.=)
    )
import Data.Text (Text)

data MnemonicsPhase = EncryptedS | DecryptedS

data Mnemonics phase where
    ClearText :: Text -> Mnemonics 'DecryptedS
    Encrypted :: Text -> Mnemonics 'EncryptedS

instance ToJSON (Mnemonics phase) where
    toJSON (ClearText mnemonics) =
        object ["mnemonics" .= mnemonics]
    toJSON (Encrypted mnemonics) =
        object ["encryptedMnemonics" .= mnemonics]
