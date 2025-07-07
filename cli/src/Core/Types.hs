{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Types
    ( Address (..)
    , CageDatum (..)
    , Change (..)
    , Directory (..)
    , Host (..)
    , Key (..)
    , Operation (..)
    , Owner (..)
    , Platform (..)
    , Port (..)
    , PublicKeyHash (..)
    , Repository (..)
    , RequestRefId (..)
    , Role (..)
    , Root (..)
    , SHA1 (..)
    , SignedTx (..)
    , SignTxError (..)
    , TokenId (..)
    , TxHash (..)
    , UnsignedTx (..)
    , Username (..)
    , Wallet (..)
    , WithTxHash (..)
    , WithUnsignedTx (..)
    ) where

import Control.Exception (Exception)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Lib.JSON
    ( object
    , parseJSValue
    , withObject
    , (.:)
    , (.=)
    )
import PlutusTx (Data (..), builtinDataToData)
import PlutusTx.IsData.Class
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    , expectedButGotValue
    , renderCanonicalJSON
    )

-- TxHash-OutputIndex
newtype RequestRefId = RequestRefId
    { requestId :: Text
    }
    deriving (Eq, Show)

instance ToHttpApiData RequestRefId where
    toUrlPiece (RequestRefId rid) = rid

instance FromHttpApiData RequestRefId where
    parseUrlPiece rid =
        case rid of
            "" -> Left "RequestRefId cannot be empty"
            _ -> Right (RequestRefId rid)

instance Monad m => ToJSON m RequestRefId where
    toJSON (RequestRefId ref) = toJSON ref

instance ReportSchemaErrors m => FromJSON m RequestRefId where
    fromJSON v = RequestRefId <$> fromJSON v

instance Aeson.ToJSON RequestRefId where
    toJSON (RequestRefId ref) = Aeson.String ref

instance Aeson.FromJSON RequestRefId where
    parseJSON = Aeson.withText "RequestRefId" $ \txt ->
        pure $ RequestRefId txt

newtype TokenId = TokenId String
    deriving (Eq, Show)

instance Monad m => ToJSON m TokenId where
    toJSON (TokenId tokenId) = toJSON tokenId

instance ReportSchemaErrors m => FromJSON m TokenId where
    fromJSON v = TokenId <$> fromJSON v

instance FromData TokenId where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 0 [B b] -> Just (TokenId $ B.unpack $ encode b)
            _ -> Nothing

instance ToHttpApiData TokenId where
    toUrlPiece (TokenId tokenId) = T.pack tokenId

instance FromHttpApiData TokenId where
    parseUrlPiece tokenId =
        case T.unpack tokenId of
            "" -> Left "TokenId cannot be empty"
            _ -> Right (TokenId (T.unpack tokenId))

newtype Owner = Owner String
    deriving (Eq, Show)

instance Monad m => ToJSON m Owner where
    toJSON (Owner owner) = toJSON owner

instance ReportSchemaErrors m => FromJSON m Owner where
    fromJSON v = Owner <$> fromJSON v

instance FromData Owner where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            B b -> Just (Owner $ B.unpack $ encode b)
            _ -> Nothing

newtype Key a = Key a
    deriving (Eq, Show)

instance FromJSON Maybe a => FromData (Key a) where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            B b -> Key <$> parseJSValue b
            _ -> Nothing

instance (ToJSON m a, Monad m) => ToJSON m (Key a) where
    toJSON (Key key) = do
        j <- BL.unpack . renderCanonicalJSON <$> toJSON key
        toJSON j

instance
    (FromJSON m a, Monad m, ReportSchemaErrors m)
    => FromJSON m (Key a)
    where
    fromJSON v = do
        keyString :: String <- fromJSON v
        Key <$> parseJSValue (B.pack keyString)

data Operation a
    = Insert a
    | Delete a
    | Update a a
    deriving (Eq, Show)

instance FromJSON Maybe a => FromData (Operation a) where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 0 [B b] -> Insert <$> parseJSValue b
            Constr 1 [B b] -> Delete <$> parseJSValue b
            Constr 2 [B old, B new] -> do
                oldVal <- parseJSValue old
                newVal <- parseJSValue new
                Just (Update oldVal newVal)
            _ -> Nothing

instance (ToJSON m a, Monad m) => ToJSON m (Operation a) where
    toJSON (Insert a) = do
        v <- BL.unpack . renderCanonicalJSON <$> toJSON a
        object
            ["type" .= ("insert" :: String), "value" .= v]
    toJSON (Delete a) = do
        v <- BL.unpack . renderCanonicalJSON <$> toJSON a
        object
            ["type" .= ("delete" :: String), "value" .= v]
    toJSON (Update old new) = do
        oldValue <- BL.unpack . renderCanonicalJSON <$> toJSON old
        newValue <- BL.unpack . renderCanonicalJSON <$> toJSON new
        object
            [ "type" .= ("update" :: String)
            , "oldValue" .= oldValue
            , "newValue" .= newValue
            ]

instance (ReportSchemaErrors m, FromJSON m a) => FromJSON m (Operation a) where
    fromJSON = withObject "Operation" $ \v -> do
        op <- v .: "type"
        case op of
            JSString "insert" -> do
                valueString <- v .: "value"
                Insert <$> parseJSValue (B.pack valueString)
            JSString "delete" -> do
                valueString <- v .: "value"
                Delete <$> parseJSValue (B.pack valueString)
            JSString "update" -> do
                oldValueString <- v .: "oldValue"
                oldValue <- parseJSValue (B.pack oldValueString)
                newValueString <- v .: "newValue"
                newValue <- parseJSValue (B.pack newValueString)
                pure $ Update oldValue newValue
            _ -> expectedButGotValue "Operation" op

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

data Change k v = Change
    { key :: Key k
    , operation :: Operation v
    }
    deriving (Eq, Show)

instance
    (FromJSON m k, FromJSON m v, ReportSchemaErrors m)
    => FromJSON m (Change k v)
    where
    fromJSON w = flip (withObject "Change") w $ \v -> do
        key <- v .: "key"
        operation <- fromJSON w
        pure $ Change key operation

instance
    (Monad m, ToJSON m k, ToJSON m v)
    => ToJSON m (Change k v)
    where
    toJSON (Change key operation) =
        object
            [ "key" .= key
            , "operation" .= operation
            ]

data CageDatum k v
    = RequestDatum
        { tokenId :: TokenId
        , owner :: Owner
        , change :: Change k v
        }
    | StateDatum
        { owner :: Owner
        , root :: Root
        }
    deriving (Eq, Show)

instance (FromJSON Maybe k, FromJSON Maybe v) => FromData (CageDatum k v) where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 0 [tokenIdD, ownerD, keyD, valueD] ->
                RequestDatum
                    <$> fromData tokenIdD
                    <*> fromData ownerD
                    <*> (Change <$> fromData keyD <*> fromData valueD)
            Constr 1 [ownerD, rootD] ->
                StateDatum
                    <$> fromData ownerD
                    <*> fromData rootD
            _ -> Nothing

newtype Platform = Platform String
    deriving (Eq, Show)

newtype PublicKeyHash = PublicKeyHash String
    deriving (Eq, Show)

newtype SHA1 = SHA1 String
    deriving (Eq, Show)

newtype Role = Role String
    deriving (Eq, Show)

newtype Directory = Directory String
    deriving (Eq, Show)

newtype Username = Username String
    deriving (Eq, Show)

data Repository = Repository
    { organization :: String
    , project :: String
    }
    deriving (Eq, Show)

newtype Port = Port Int
    deriving (Eq, Show)

newtype Host = Host String
    deriving (Eq, Show)

newtype Address = Address Text
    deriving (Eq, Show)

instance Monad m => ToJSON m Address where
    toJSON (Address addr) = toJSON addr

instance ReportSchemaErrors m => FromJSON m Address where
    fromJSON v = Address <$> fromJSON v

instance FromHttpApiData Address where
    parseUrlPiece addr =
        if T.null addr
            then Left "Address cannot be empty"
            else Right (Address addr)

instance ToHttpApiData Address where
    toUrlPiece (Address addr) = addr

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
    deriving (Show)

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

data Wallet = Wallet
    { address :: Address
    , owner :: Owner
    , sign :: UnsignedTx -> Either SignTxError SignedTx
    }
