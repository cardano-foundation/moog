{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JSON.Canonical.Extra
    ( getField
    , getStringField
    , getIntegralField
    , getStringMapField
    , object
    , intJSON
    , stringJSON
    , getListField
    , jsonToString
    , CanonicalJSON (..)
    , CanonicalJSONError (..)
    , runIdentityCanonicalJSON
    , withObject
    , (.:)
    , (.=)
    , (.:?)
    , fromAeson
    , fromAesonThrow
    , toAeson
    , toAesonString
    , fromAesonString
    , parseJSValue
    , byteStringToJSON
    , byteStringFromJSON
    , mergeObject
    , blakeHashOfJSON
    , getFieldWithDefault
    )
where

import Control.Monad ((<=<))
import Crypto.Hash (Blake2b_256, Digest)
import Crypto.Hash qualified as Hash
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonInternal
import Data.Bifunctor (first)
import Data.ByteArray qualified as BA
import Data.ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Lib.JSON.Canonical.Aeson qualified as CA
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , Int54
    , JSValue (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    , expectedButGotValue
    , fromJSString
    , parseCanonicalJSON
    , renderCanonicalJSON
    )

getField
    :: (ReportSchemaErrors m, FromJSON m a)
    => String
    -> Map String JSValue
    -> m a
getField key mapping = case Map.lookup key mapping of
    Nothing -> expected (key <> " key") Nothing
    Just value -> fromJSON value

getFieldWithDefault
    :: (ReportSchemaErrors m, FromJSON m a)
    => String
    -> a
    -> Map String JSValue
    -> m a
getFieldWithDefault key def mapping =
    case Map.lookup key mapping of
        Nothing -> pure def
        Just value -> fromJSON value

getStringField
    :: ReportSchemaErrors m
    => String
    -> Map String JSValue
    -> m String
getStringField = getField

getListField
    :: ReportSchemaErrors m
    => String
    -> Map String JSValue
    -> m [JSValue]
getListField = getField

getIntegralField
    :: (ReportSchemaErrors m, Num a)
    => String
    -> Map String JSValue
    -> m a
getIntegralField key mapping =
    fromIntegral @Int54 <$> getField key mapping

getStringMapField
    :: ReportSchemaErrors m
    => String
    -> Map String JSValue
    -> m (Map String JSValue)
getStringMapField = getField

object :: (Monad m, ToJSON m a) => [(String, m a)] -> m JSValue
object xs = toJSON <=< sequence $ Map.fromList xs

mergeObject :: JSValue -> JSValue -> JSValue
mergeObject (JSObject a) (JSObject b) = JSObject (a ++ b)
mergeObject _ _ = error "Can only merge two JSObjects"

intJSON :: (Monad m, Integral a) => a -> m JSValue
intJSON = toJSON @_ @Int54 . fromIntegral

stringJSON :: Monad m => String -> m JSValue
stringJSON = toJSON

jsonToString :: JSValue -> String
jsonToString = BL.unpack . renderCanonicalJSON

byteStringToJSON :: Monad m => ByteString -> m JSValue
byteStringToJSON = toJSON . B.unpack . Base16.encode

byteStringFromJSON
    :: (ReportSchemaErrors m) => JSValue -> m ByteString
byteStringFromJSON s = do
    string <- fromJSON s
    pure $ Base16.decodeLenient . B.pack $ string

toAeson :: JSValue -> Value
toAeson = CA.fromCanon

data CanonicalJSONError = CanonicalJSONError
    { expectedValue :: String
    , gotValue :: Maybe String
    }
    deriving (Show, Eq)

-- >>> import Data.Functor.Identity
-- >>> pure 1 :: CanonicalJSON Identity Int
newtype CanonicalJSON m a = CanonicalJSON
    {runCanonicalJSON :: m (Either CanonicalJSONError a)}
    deriving (Functor)

instance Applicative m => Applicative (CanonicalJSON m) where
    pure x = CanonicalJSON $ pure $ Right x
    CanonicalJSON f <*> CanonicalJSON x = CanonicalJSON $ liftA2 (<*>) f x

instance Monad m => Monad (CanonicalJSON m) where
    CanonicalJSON x >>= f = CanonicalJSON $ do
        result <- x
        case result of
            Left err -> pure $ Left err
            Right value -> runCanonicalJSON $ f value

instance Monad m => ReportSchemaErrors (CanonicalJSON m) where
    expected expectedValue gotValue =
        CanonicalJSON
            $ pure
            $ Left
            $ CanonicalJSONError{expectedValue, gotValue}

runIdentityCanonicalJSON
    :: CanonicalJSON Identity a -> Either CanonicalJSONError a
runIdentityCanonicalJSON (CanonicalJSON x) = runIdentity x

withObject
    :: (ReportSchemaErrors m)
    => String
    -> (Map String JSValue -> m a)
    -> JSValue
    -> m a
withObject _ f (JSObject mapping) =
    f $ Map.fromList $ mapping <&> first fromJSString
withObject name _ v = expectedButGotValue name v

(.:)
    :: (ReportSchemaErrors m, FromJSON m a)
    => Map String JSValue
    -> String
    -> m a
mapping .: key = getField key mapping >>= fromJSON

(.:?)
    :: (ReportSchemaErrors m, FromJSON m a)
    => Map String JSValue
    -> String
    -> m (Maybe a)
mapping .:? key = getField key mapping >>= fromJSON <&> Just

(.=) :: ToJSON m a => String -> a -> (String, m JSValue)
key .= value = (key, toJSON value)

instance Monad m => ToJSON m Text where
    toJSON = toJSON . T.unpack

instance (Monad m, ReportSchemaErrors m) => FromJSON m Text where
    fromJSON = fmap T.pack . fromJSON

instance (Monad m, ToJSON m a) => ToJSON m (Maybe a) where
    toJSON Nothing = pure JSNull
    toJSON (Just a) = toJSON a

instance (ReportSchemaErrors m, FromJSON m a) => FromJSON m (Maybe a) where
    fromJSON JSNull = pure Nothing
    fromJSON v = Just <$> fromJSON v

fromAeson :: Value -> Either String JSValue
fromAeson v = case CA.fromAeson v of
    Just js -> Right js
    Nothing -> Left "nothing"

fromAesonThrow :: Value -> JSValue
fromAesonThrow value =
    case fromAeson value of
        Left err ->
            error $ "Failed to convert Aeson Value to JSValue: " ++ err
        Right jsValue -> jsValue

instance Applicative m => ToJSON m Value where
    toJSON = pure . fromAesonThrow

toAesonString :: JSValue -> Value
toAesonString = Aeson.String . T.decodeUtf8 . BL.toStrict . renderCanonicalJSON

fromAesonString :: Value -> AesonInternal.Parser JSValue
fromAesonString = Aeson.withText "JSValue" $ \v ->
    case parseCanonicalJSON (BL.fromStrict $ T.encodeUtf8 v) of
        Left err -> fail $ "Failed to parse value: " ++ err
        Right jsValue -> pure jsValue

instance {-# OVERLAPPING #-} ReportSchemaErrors Maybe where
    expected _expectedValue _gotValue = Nothing

parseJSValue
    :: (FromJSON m a, ReportSchemaErrors m)
    => StrictByteString
    -> m a
parseJSValue b = do
    js <- case parseCanonicalJSON (BL.fromStrict b) of
        Left err -> expected "JSValue" (Just err)
        Right js -> pure js
    fromJSON js

instance Applicative m => ToJSON m () where
    toJSON () = pure JSNull

instance (ReportSchemaErrors m) => FromJSON m () where
    fromJSON JSNull = pure ()
    fromJSON v = expectedButGotValue "()" v

instance ToJSON m a => ToJSON m (Identity a) where
    toJSON (Identity a) = toJSON a

instance (ReportSchemaErrors m, FromJSON m a) => FromJSON m (Identity a) where
    fromJSON v = Identity <$> fromJSON v

blakeHashOfJSON
    :: (ToJSON m a, Monad m) => a -> m B.ByteString
blakeHashOfJSON a = do
    aJ <- toJSON a
    pure
        $ BA.convert @(Digest Blake2b_256)
        $ Hash.hash
        $ BL.toStrict
        $ renderCanonicalJSON aJ
