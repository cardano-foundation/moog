{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Core.Types.Fact
    ( Fact (..)
    , Slot (..)
    , keyHash
    , parseFacts
    , JSFact
    , toJSFact
    , fromJSFact
    , renderFacts
    ) where

import Control.Lens (Identity (runIdentity))
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Lib.JSON.Canonical.Extra
    ( blakeHashOfJSON
    , object
    , parseJSValue
    , renderJSValue
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
    deriving (Eq, Show, Functor, Foldable, Traversable, Ord)

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

data RawValue = RawValue
    { _rawValue :: String
    , _rawSlot :: Slot
    }
    deriving (Eq, Show)

instance (ReportSchemaErrors m) => FromJSON m RawValue where
    fromJSON = withObject "RawValue" $ \v -> do
        value <- v .: "value"
        slot <- v .: "slot"
        pure $ RawValue value slot

parseFacts
    :: (FromJSON Maybe key, FromJSON Maybe val) => JSValue -> [Fact key val]
parseFacts v = fromMaybe [] $ do
    factsJSON <- fromJSON v
    pure $ mapMaybe f $ M.assocs factsJSON
  where
    f (key, RawValue value slot) = do
        key' <- parseJSValue (B8.pack key)
        value' <- parseJSValue (B8.pack value)
        Just $ Fact key' value' slot

renderFacts
    :: forall k v
     . (ToJSON Identity k, ToJSON Identity v)
    => [Fact k v]
    -> JSValue
renderFacts facts = runIdentity $ object =<< traverse f facts
  where
    f :: Fact k v -> Identity (String, Identity JSValue)
    f (Fact key value slot) = do
        key' <- B8.unpack <$> renderJSValue key
        value' <- B8.unpack <$> renderJSValue value
        pure
            ( key'
            , object
                [ "value" .= value'
                , "slot" .= slot
                ]
            )

type JSFact = Fact JSValue JSValue

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
