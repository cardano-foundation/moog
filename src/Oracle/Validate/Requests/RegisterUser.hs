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
    ( VKeyFailure (..)
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
    ( GithubIdentification (..)
    , RegisterUserKey (..)
    )

data RegisterUserFailure
    = SSHKeyRegistrationDeprecated
    | VKeyValidationFailure VKeyFailure
    | RegisterUserPlatformNotSupported String
    | RegisterUserKeyFailure KeyFailure
    | RegisterUserKeyChangeAlreadyPending RegisterUserKey
    | RegisterUserKeyAlreadyExists GithubIdentification
    deriving (Show, Eq)

instance Monad m => ToJSON m RegisterUserFailure where
    toJSON = \case
        SSHKeyRegistrationDeprecated ->
            object
                [ "sshKeyRegistrationDeprecated"
                    .= ( "SSH key registration is deprecated and no longer supported. Please use wallet vkey registration."
                            :: String
                       )
                ]
        VKeyValidationFailure reason ->
            object ["vKeyValidationFailure" .= reason]
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
        , githubEffects = GithubEffects{githubUserVKeys}
        }
    forRole
    change@( Change
                (Key key@(RegisterUserKey{platform, username, githubIdentification}))
                _
            ) =
        case platform of
            Platform "github" -> do
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
                case find (\(Fact k' _ _) -> matchUsername k') users of
                    Just (Fact (RegisterUserKey _ _ k) _ _) ->
                        notValidated $ RegisterUserKeyAlreadyExists k
                    Nothing -> do
                        case githubIdentification of
                            IdentifyViaSSHKey _sshKey ->
                                notValidated SSHKeyRegistrationDeprecated
                            IdentifyViaVKey vKey -> do
                                validationRes <- lift $ githubUserVKeys username vKey
                                mapFailure VKeyValidationFailure
                                    $ throwJusts validationRes
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
    validation@Effects
        { githubEffects = GithubEffects{githubUserVKeys}
        }
    forRole
    change@( Change
                (Key key@(RegisterUserKey{platform, username, githubIdentification}))
                _v
            ) =
        case platform of
            Platform "github" -> do
                when (forUser forRole)
                    $ keyAlreadyPendingFailure
                        validation
                        UnregisterUserKeyChangeAlreadyPending
                        key
                        requestZooGetRegisterUserKey
                void
                    $ mapFailure UnregisterUserKeyFailure
                    $ deleteValidation validation change
                case githubIdentification of
                    IdentifyViaVKey vkey -> do
                        validationRes <- lift $ githubUserVKeys username vkey
                        case validationRes of
                            Just VKeyNotFound -> pure Validated
                            Just VKeyMismatch{} -> pure Validated
                            Just (VKeyGithubError err) -> notValidated $ UnregisterUserKeyGithubError $ show err
                            Nothing -> notValidated UnregisterUserKeyIsPresent
                    IdentifyViaSSHKey _ssh -> pure Validated -- Open up the possibility to unregister even if the key is still present to ease migration to vkey
            Platform other -> notValidated $ UnregisterUserPlatformNotSupported other
