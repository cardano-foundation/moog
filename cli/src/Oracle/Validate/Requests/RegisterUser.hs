module Oracle.Validate.Requests.RegisterUser
    ( validateRegisterUser
    , validateUnregisterUser
    , RegisterUserFailure (..)
    , UnregisterUserFailure (..)
    ) where

import Control.Monad (void, when)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic
    ( Platform (..)
    , PublicKeyHash (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Core.Types.Operation (Op (..))
import Data.List (find)
import Effects
    ( Effects (..)
    , GithubEffects (..)
    , KeyFailure
    , deleteValidation
    , insertValidation
    )
import Effects.RegisterUser
    ( PublicKeyFailure (..)
    )
import Lib.JSON.Canonical.Extra (object, (.=))
import Oracle.Types (requestZooGetRegisterUserKey)
import Oracle.Validate.Requests.Lib (keyAlreadyPendingFailure)
import Oracle.Validate.Types
    ( ForRole
    , Validate
    , Validated (..)
    , forUser
    , mapFailure
    , notValidated
    , throwJusts
    )
import Text.JSON.Canonical (ToJSON (..))
import User.Types
    ( RegisterUserKey (..)
    )

data RegisterUserFailure
    = PublicKeyValidationFailure PublicKeyFailure
    | RegisterUserPlatformNotSupported String
    | RegisterUserKeyFailure KeyFailure
    | RegisterUserKeyChangeAlreadyPending RegisterUserKey
    | RegisterUserKeyAlreadyExists String
    deriving (Show, Eq)

instance Monad m => ToJSON m RegisterUserFailure where
    toJSON = \case
        PublicKeyValidationFailure reason ->
            object ["publicKeyValidationFailure" .= reason]
        RegisterUserPlatformNotSupported platform ->
            object ["registerUserPlatformNotSupported" .= platform]
        RegisterUserKeyFailure keyFailure ->
            object ["registerUserKeyFailure" .= keyFailure]
        RegisterUserKeyChangeAlreadyPending key ->
            object ["registerUserKeyChangeAlreadyPending" .= key]
        RegisterUserKeyAlreadyExists pkHash ->
            object ["registerUserKeyAlreadyExists" .= pkHash]

validateRegisterUser
    :: Monad m
    => Effects m
    -> ForRole
    -> Change RegisterUserKey (OpI ())
    -> Validate RegisterUserFailure m Validated
validateRegisterUser
    validation@Effects
        { mpfsGetFacts
        , githubEffects = GithubEffects{githubUserPublicKeys}
        }
    forRole
    change@(Change (Key key@(RegisterUserKey{platform, username, pubkeyhash})) _) = do
        when (forUser forRole)
            $ keyAlreadyPendingFailure
                validation
                RegisterUserKeyChangeAlreadyPending
                key
                requestZooGetRegisterUserKey
        mapFailure RegisterUserKeyFailure $ insertValidation validation change
        users :: [Fact RegisterUserKey ()] <- lift mpfsGetFacts
        let matchUsername (RegisterUserKey platform' username' _) =
                platform' == platform && username' == username
        case find (\(Fact k' _) -> matchUsername k') users of
            Just (Fact (RegisterUserKey _ _ (PublicKeyHash pkHash)) _) ->
                notValidated $ RegisterUserKeyAlreadyExists pkHash
            Nothing -> pure ()
        case platform of
            Platform "github" -> do
                validationRes <- lift $ githubUserPublicKeys username pubkeyhash
                mapFailure PublicKeyValidationFailure $ throwJusts validationRes
            Platform other -> notValidated $ RegisterUserPlatformNotSupported other

data UnregisterUserFailure
    = UnregisterUserPlatformNotSupported String
    | UnregisterUserKeyFailure KeyFailure
    | UnregisterUserKeyChangeAlreadyPending RegisterUserKey
    | UnregisterUserKeyIsPresent
    | UnregisterUserKeyGithubError String
    deriving (Show, Eq)

instance Monad m => ToJSON m UnregisterUserFailure where
    toJSON = \case
        UnregisterUserPlatformNotSupported platform ->
            object ["unregisterUserPlatformNotSupported" .= platform]
        UnregisterUserKeyFailure keyFailure ->
            object ["unregisterUserKeyFailure" .= keyFailure]
        UnregisterUserKeyChangeAlreadyPending key ->
            object ["unregisterUserKeyChangeAlreadyPending" .= key]
        UnregisterUserKeyIsPresent ->
            object
                [ "unregisterUserKeyIsPresent"
                    .= ("The user still has the public key present in Github." :: String)
                ]
        UnregisterUserKeyGithubError err ->
            object ["unregisterUserKeyGithubError" .= err]

validateUnregisterUser
    :: Monad m
    => Effects m
    -> ForRole
    -> Change RegisterUserKey (OpD ())
    -> Validate UnregisterUserFailure m Validated
validateUnregisterUser
    validation@Effects{githubEffects = GithubEffects{githubUserPublicKeys}}
    forRole
    change@(Change (Key key@(RegisterUserKey{platform, username, pubkeyhash})) _v) = do
        when (forUser forRole)
            $ keyAlreadyPendingFailure
                validation
                UnregisterUserKeyChangeAlreadyPending
                key
                requestZooGetRegisterUserKey
        void
            $ mapFailure UnregisterUserKeyFailure
            $ deleteValidation validation change
        case platform of
            Platform "github" -> do
                validationRes <- lift $ githubUserPublicKeys username pubkeyhash
                case validationRes of
                    Just NoEd25519KeyFound -> pure Validated
                    Just NoEd25519KeyMatch -> pure Validated
                    Just NoPublicKeyFound -> pure Validated
                    Just (GithubError err) -> notValidated $ UnregisterUserKeyGithubError err
                    Nothing -> notValidated UnregisterUserKeyIsPresent
            Platform other -> notValidated $ UnregisterUserPlatformNotSupported other
