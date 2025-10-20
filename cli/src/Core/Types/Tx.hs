{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Core.Types.Tx
    ( Root (..)
    , SignedTx (..)
    , SignTxError (..)
    , TxHash (..)
    , UnsignedTx (..)
    , WithTxHash (..)
    , WithUnsignedTx (..)
    , setWithTxHashValue
    ) where

import Control.Exception (Exception)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Lib.JSON.Canonical.Extra
    ( object
    , withObject
    , (.:)
    , (.=)
    )
import PlutusTx (Data (..), builtinDataToData)
import PlutusTx.IsData.Class (FromData (..))
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.JSON.Canonical
    ( FromJSON (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    )

newtype Root = Root String
    deriving (Eq, Show)

instance FromData Root where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            B b -> Just (Root $ B.unpack $ encode b)
            _ -> Nothing

instance Monad m => ToJSON m Root where
    toJSON (Root root) = toJSON root

instance (ReportSchemaErrors m) => FromJSON m Root where
    fromJSON v = Root <$> fromJSON v

newtype SignedTx = SignedTx
    { signedTransaction :: Text
    }
    deriving (Show)

instance Aeson.FromJSON SignedTx where
    parseJSON = Aeson.withObject "SignedTx" $ \v ->
        SignedTx
            <$> v Aeson..: "signedTransaction"

instance Aeson.ToJSON SignedTx where
    toJSON (SignedTx signedTransaction) =
        Aeson.object
            [ "signedTransaction" Aeson..= signedTransaction
            ]
newtype UnsignedTx = UnsignedTx
    { unsignedTransaction :: Text
    }
    deriving (Show, Eq)

instance Aeson.FromJSON UnsignedTx where
    parseJSON = Aeson.withText "UnsignedTx" $ \v ->
        pure (UnsignedTx v)

instance Aeson.ToJSON UnsignedTx where
    toJSON (UnsignedTx unsignedTransaction) =
        Aeson.toJSON unsignedTransaction

instance Monad m => ToJSON m UnsignedTx where
    toJSON (UnsignedTx unsignedTransaction) =
        object
            [ "unsignedTransaction" .= unsignedTransaction
            ]

instance ReportSchemaErrors m => FromJSON m UnsignedTx where
    fromJSON = withObject "UnsignedTx" $ \v ->
        UnsignedTx
            <$> v .: "unsignedTransaction"

data WithUnsignedTx a = WithUnsignedTx
    { unsignedTransaction :: UnsignedTx
    , value :: Maybe a
    }
    deriving (Show, Functor, Eq)

instance (ToJSON m a, Monad m) => ToJSON m (WithUnsignedTx a) where
    toJSON (WithUnsignedTx unsignedTransaction value) =
        object
            [ "unsignedTransaction" .= unsignedTransaction
            , "value" .= value
            ]

instance (Aeson.FromJSON a) => Aeson.FromJSON (WithUnsignedTx a) where
    parseJSON = Aeson.withObject "WithUnsignedTx" $ \v ->
        WithUnsignedTx
            <$> v Aeson..: "unsignedTransaction"
            <*> v Aeson..:? "value"

instance (Aeson.ToJSON a) => Aeson.ToJSON (WithUnsignedTx a) where
    toJSON (WithUnsignedTx unsignedTransaction value) =
        Aeson.object
            [ "unsignedTransaction" Aeson..= unsignedTransaction
            , "value" Aeson..= value
            ]

newtype TxHash = TxHash
    { textOf :: Text
    }
    deriving (Eq, Show)

instance FromHttpApiData TxHash where
    parseUrlPiece textOf =
        if T.null textOf
            then Left "TxHash cannot be empty"
            else Right (TxHash{textOf})

instance ToHttpApiData TxHash where
    toUrlPiece (TxHash txHash) = txHash
instance Aeson.FromJSON TxHash where
    parseJSON = Aeson.withObject "TxHash" $ \v ->
        TxHash
            <$> v Aeson..: "txHash"

instance Aeson.ToJSON TxHash where
    toJSON (TxHash txHash) =
        Aeson.object
            [ "txHash" Aeson..= txHash
            ]

instance Monad m => ToJSON m TxHash where
    toJSON (TxHash txHash) =
        object
            [ "txHash" .= txHash
            ]
data WithTxHash a = WithTxHash
    { txHash :: TxHash
    , value :: Maybe a
    }
    deriving (Show, Functor, Eq)

setWithTxHashValue :: WithTxHash a -> b -> WithTxHash b
setWithTxHashValue (WithTxHash txHash _) newValue =
    WithTxHash txHash (Just newValue)

instance (Monad m, ToJSON m a) => ToJSON m (WithTxHash a) where
    toJSON (WithTxHash (TxHash txHash) value) =
        object
            [ "txHash" .= txHash
            , "value" .= value
            ]

data SignTxError
    = InvalidTx
    | InvalidHex
    deriving (Show, Eq)

instance Exception SignTxError
