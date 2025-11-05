{-# LANGUAGE StrictData #-}

module Core.Types.Operation
    ( Operation (..)
    , Op (..)
    ) where

import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Lib.JSON.Canonical.Extra
    ( object
    , parseJSValue
    , withObject
    , (.:)
    , (.=)
    )
import PlutusTx (Data (..), builtinDataToData)
import PlutusTx.IsData.Class (FromData (..))
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    , expectedButGotValue
    , renderCanonicalJSON
    )

data Op a b = OpI a | OpD b | OpU a b
    deriving (Show, Eq)

data Operation s where
    Insert :: a -> Operation (OpI a)
    Delete :: b -> Operation (OpD b)
    Update :: a -> b -> Operation (OpU a b)

deriving instance Show a => Show (Operation (OpI a))
deriving instance Show a => Show (Operation (OpD a))
deriving instance (Show a, Show b) => Show (Operation (OpU a b))

deriving instance Eq a => Eq (Operation (OpI a))
deriving instance Eq a => Eq (Operation (OpD a))
deriving instance (Eq a, Eq b) => Eq (Operation (OpU a b))

instance FromJSON Maybe a => FromData (Operation (OpI a)) where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 0 [B b] -> Insert <$> parseJSValue b
            _ -> Nothing
instance FromJSON Maybe a => FromData (Operation (OpD a)) where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 1 [B b] -> Delete <$> parseJSValue b
            _ -> Nothing

instance (FromJSON Maybe a, FromJSON Maybe b) => FromData (Operation (OpU a b)) where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 2 [B old, B new] -> do
                oldValue <- parseJSValue old
                newValue <- parseJSValue new
                pure $ Update oldValue newValue
            _ -> Nothing

instance (ToJSON m a, Monad m) => ToJSON m (Operation (OpI a)) where
    toJSON (Insert a) = do
        v <- BL.unpack . renderCanonicalJSON <$> toJSON a
        object
            ["type" .= ("insert" :: String), "newValue" .= v]

instance (ToJSON m a, Monad m) => ToJSON m (Operation (OpD a)) where
    toJSON (Delete a) = do
        v <- BL.unpack . renderCanonicalJSON <$> toJSON a
        object
            ["type" .= ("delete" :: String), "oldValue" .= v]

instance (ToJSON m a, ToJSON m b, Monad m) => ToJSON m (Operation (OpU a b)) where
    toJSON (Update old new) = do
        oldValue <- BL.unpack . renderCanonicalJSON <$> toJSON old
        newValue <- BL.unpack . renderCanonicalJSON <$> toJSON new
        object
            [ "type" .= ("update" :: String)
            , "oldValue" .= oldValue
            , "newValue" .= newValue
            ]

instance
    (ReportSchemaErrors m, FromJSON m a)
    => FromJSON m (Operation (OpI a))
    where
    fromJSON = withObject "Operation" $ \v -> do
        op <- v .: "type"
        case op of
            JSString "insert" -> do
                valueString <- v .: "newValue"
                Insert <$> parseJSValue (B.pack valueString)
            _ -> expectedButGotValue "insert operation" op

instance
    (ReportSchemaErrors m, FromJSON m a)
    => FromJSON m (Operation (OpD a))
    where
    fromJSON = withObject "Operation" $ \v -> do
        op <- v .: "type"
        case op of
            JSString "delete" -> do
                valueString <- v .: "oldValue"
                Delete <$> parseJSValue (B.pack valueString)
            _ -> expectedButGotValue "delete operation" op

instance
    (ReportSchemaErrors m, FromJSON m a, FromJSON m b)
    => FromJSON m (Operation (OpU a b))
    where
    fromJSON = withObject "Operation" $ \v -> do
        op <- v .: "type"
        case op of
            JSString "update" -> do
                oldValueString <- v .: "oldValue"
                oldValue <- parseJSValue (B.pack oldValueString)
                newValueString <- v .: "newValue"
                newValue <- parseJSValue (B.pack newValueString)
                pure $ Update oldValue newValue
            _ -> expectedButGotValue "Operation" op
