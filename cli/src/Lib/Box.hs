{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Box
    ( Box (..)
    , fmapBox
    ) where

import Text.JSON.Canonical (ToJSON)

data Box f = forall a. (forall m. Monad m => ToJSON m a) => Box (f a)

instance (forall a. Show (f a)) => Show (Box f) where
    show (Box x) = show x

instance (forall a. Show (f a)) => Eq (Box f) where
    Box x == Box y = show x == show y

fmapBox :: (forall a. f1 a -> f2 a) -> Box f1 -> Box f2
fmapBox f (Box x) = Box (f x)
