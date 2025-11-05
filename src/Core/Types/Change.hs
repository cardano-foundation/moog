{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Types.Change
    ( Change (..)
    , Key (..)
    , insertKey
    , deleteKey
    ) where

import Core.Types.Operation (Op (..), Operation (..))
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Lib.JSON.Canonical.Extra
    ( mergeObject
    , object
    , parseJSValue
    , withObject
    , (.:)
    , (.=)
    )
import PlutusTx (Data (..), builtinDataToData)
import PlutusTx.IsData.Class (FromData (..))
import Text.JSON.Canonical
    ( FromJSON (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    , renderCanonicalJSON
    )

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

data Change k op = Change
    { key :: Key k
    , operation :: Operation op
    }

deriving instance (Show k, Show (Operation op)) => Show (Change k op)
deriving instance (Eq k, Eq (Operation op)) => Eq (Change k op)

instance
    (FromJSON m k, FromJSON m (Operation op), ReportSchemaErrors m)
    => FromJSON m (Change k op)
    where
    fromJSON w = flip (withObject "Change") w $ \v -> do
        key <- v .: "key"
        operation <- fromJSON w
        pure $ Change key operation

instance
    (Monad m, ToJSON m k, ToJSON m (Operation op))
    => ToJSON m (Change k op)
    where
    toJSON (Change key operation) = do
        operationJ <- toJSON operation
        keyJ <-
            object
                [ "key" .= key
                ]
        pure $ keyJ `mergeObject` operationJ

insertKey :: a -> Change a (OpI ())
insertKey key = Change{key = Key key, operation = Insert ()}

deleteKey :: a -> Change a (OpD ())
deleteKey key = Change{key = Key key, operation = Delete ()}
