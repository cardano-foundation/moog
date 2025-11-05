{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Oracle.Validate.Types
    ( ValidationResult
    , Validate
    , AValidationResult (..)
    , Validated (..)
    , runValidate
    , notValidated
    , mapFailure
    , throwJusts
    , withValidationResult
    , throwValidationResult
    , sequenceValidate
    , throwFalse
    , throwLeft
    , liftMaybe
    , hoistValidate
    , ForRole (..)
    , forUser
    ) where

import Control.Monad.Catch (MonadCatch, MonadMask (..), MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadTransControl (..))
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExceptT
    , throwE
    , withExceptT
    )
import Lib.JSON.Canonical.Extra
    ( object
    , (.=)
    )
import Text.JSON.Canonical.Class (ToJSON (..))

newtype Validate e m a = Validate (ExceptT e m a)
    deriving
        (Functor, Applicative, Monad, MonadMask, MonadCatch, MonadThrow)

instance MonadTransControl (Validate e) where
    type StT (Validate e) a = StT (ExceptT e) a
    liftWith f = Validate $ liftWith $ \runInBase -> f (runInBase . (\(Validate t) -> t))
    restoreT = Validate . restoreT
hoistValidate
    :: (forall x. m x -> n x) -> Validate e m a -> Validate e n a
hoistValidate f (Validate a) = Validate $ ExceptT $ f $ runExceptT a

instance MonadTrans (Validate e) where
    lift = Validate . lift

instance MonadIO m => MonadIO (Validate e m) where
    liftIO = Validate . liftIO

data AValidationResult e a
    = ValidationSuccess a
    | ValidationFailure e
    deriving (Show, Eq, Functor)

withValidationResult
    :: (e -> e')
    -> AValidationResult e a
    -> AValidationResult e' a
withValidationResult _ (ValidationSuccess a) = ValidationSuccess a
withValidationResult f (ValidationFailure e) = ValidationFailure (f e)

data Validated = Validated
    deriving (Show, Eq)

instance Monad m => ToJSON m Validated where
    toJSON _ = toJSON ("validated" :: String)

type ValidationResult e = AValidationResult e Validated

notValidated :: Monad m => e -> Validate e m a
notValidated = Validate . throwE

runValidate
    :: Functor m => Validate e m a -> m (AValidationResult e a)
runValidate (Validate f) =
    fmap (either ValidationFailure ValidationSuccess) . runExceptT $ f

throwJusts :: Monad m => Maybe e -> Validate e m Validated
throwJusts Nothing = pure Validated
throwJusts (Just e) = notValidated e

throwValidationResult
    :: Monad m => ValidationResult e -> Validate e m Validated
throwValidationResult (ValidationSuccess a) = pure a
throwValidationResult (ValidationFailure e) = notValidated e

mapFailure
    :: Functor m => (e -> e') -> Validate e m a -> Validate e' m a
mapFailure f (Validate a) = Validate $ withExceptT f a

instance (Monad m, ToJSON m a, ToJSON m e) => ToJSON m (AValidationResult e a) where
    toJSON = \case
        ValidationSuccess a -> toJSON a
        ValidationFailure e ->
            object ["validationFailed" .= e]

sequenceValidate :: Monad m => [Validate e m a] -> Validate [e] m [a]
sequenceValidate ys = do
    (xs, es) <- go ys
    if null es
        then pure xs
        else notValidated es
  where
    go (v : vs) = do
        result <- lift $ runValidate v
        (xs, es) <- go vs
        case result of
            ValidationSuccess x -> pure (x : xs, es)
            ValidationFailure e -> pure (xs, e : es)
    go [] = pure ([], [])

throwFalse :: Monad m => Bool -> e -> Validate e m Validated
throwFalse True _ = pure Validated
throwFalse False e = notValidated e

throwLeft :: Monad m => (e -> e') -> Either e a -> Validate e' m a
throwLeft f (Left e) = notValidated (f e)
throwLeft _ (Right a) = pure a

liftMaybe :: Monad m => e -> Maybe a -> Validate e m a
liftMaybe e Nothing = notValidated e
liftMaybe _ (Just a) = pure a

data ForRole = ForOracle | ForUser
    deriving (Show, Eq)

forUser :: ForRole -> Bool
forUser ForUser = True
forUser ForOracle = False
