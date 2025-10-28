{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Core.Types.Fact
    ( Fact (..)
    , keyHash
    , parseFacts
    , JSFact
    , toJSFact
    , fromJSFact
    ) where

import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as B
import Data.Maybe (fromMaybe, mapMaybe)
import Lib.JSON.Canonical.Extra
    ( blakeHashOfJSON
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , Int54
    , JSValue (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    )

newtype Slot = Slot {unSlot :: Int}
    deriving (Eq, Show, Num, Ord)

instance (Monad m) => ToJSON m Slot where
    toJSON (Slot s) = pure $ JSNum (fromIntegral s)

instance (ReportSchemaErrors m, Monad m) => FromJSON m Slot where
    fromJSON v = do
        s <- fromJSON @_ @Int54 v
        pure $ Slot (fromIntegral s)

data Fact k v = Fact
    { factKey :: k
    , factValue :: v
    , factSlot :: Slot
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

keyHash :: (ToJSON m k, Monad m) => k -> m String
keyHash key = do
    B.unpack
        . Base16.encode
        <$> blakeHashOfJSON key
instance
    ( ReportSchemaErrors m
    , FromJSON m k
    , FromJSON m v
    )
    => FromJSON m (Fact k v)
    where
    fromJSON = withObject "Fact" $ \v -> do
        key <- v .: "key"
        value <- v .: "value"
        slot <- v .: "slot"
        pure $ Fact key value slot

instance (Monad m, ToJSON m k, ToJSON m v) => ToJSON m (Fact k v) where
    toJSON (Fact key value slot) = do
        keyJ <- toJSON key
        idJ <- keyHash key
        object
            [ "key" .= keyJ
            , "value" .= value
            , "id" .= idJ
            , "slot" .= slot
            ]

type JSFact = Fact JSValue JSValue

parseFacts
    :: (FromJSON Maybe key, FromJSON Maybe val) => JSValue -> [Fact key val]
parseFacts v = fromMaybe [] $ do
    factsJSON <- fromJSON v
    pure $ mapMaybe f factsJSON
  where
    f (Fact key value slot) = do
        key' <- fromJSON key
        value' <- fromJSON value
        Just $ Fact key' value' slot

toJSFact
    :: (ToJSON m k, ToJSON m v, Monad m) => k -> v -> Slot -> m JSFact
toJSFact key value slot = do
    keyJ <- toJSON key
    valueJ <- toJSON value
    return $ Fact keyJ valueJ slot

fromJSFact
    :: (FromJSON m k, FromJSON m v, Monad m) => JSFact -> m (Fact k v)
fromJSFact (Fact key value slot) = do
    key' <- fromJSON key
    value' <- fromJSON value
    return $ Fact key' value' slot
