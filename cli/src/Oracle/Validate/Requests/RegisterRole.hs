module Oracle.Validate.Requests.RegisterRole
    ( validateRegisterRole
    , validateUnregisterRole
    , RegisterRoleFailure (..)
    , UnregisterRoleFailure (..)
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic
    ( Platform (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation
    ( Op (..)
    )
import Lib.JSON.Canonical.Extra (object, (.=))
import Oracle.Types (requestZooGetRegisterRoleKey)
import Oracle.Validate.Requests.Lib (keyAlreadyPendingFailure)
import Oracle.Validate.Types
    ( ForRole
    , Validate
    , Validated (..)
    , forUser
    , mapFailure
    , notValidated
    )
import Text.JSON.Canonical (ToJSON (..))
import User.Types
    ( RegisterRoleKey (..)
    )
import Validation
    ( KeyFailure
    , Validation (..)
    , deleteValidation
    , insertValidation
    )
import Validation.RegisterRole
    ( RepositoryRoleFailure (..)
    )

data RegisterRoleFailure
    = RoleNotPresentOnPlatform RepositoryRoleFailure
    | RegisterRolePlatformNotSupported String
    | RegisterRoleKeyFailure KeyFailure
    | RegisterRoleKeyChangeAlreadyPending RegisterRoleKey
    deriving (Eq, Show)

instance Monad m => ToJSON m RegisterRoleFailure where
    toJSON = \case
        RoleNotPresentOnPlatform reason ->
            object
                ["roleNotPresentOnPlatform" .= reason]
        RegisterRolePlatformNotSupported platform ->
            object ["registerRolePlatformNotSupported" .= platform]
        RegisterRoleKeyFailure keyFailure ->
            object ["registerRoleKeyFailure" .= keyFailure]
        RegisterRoleKeyChangeAlreadyPending key ->
            object ["registerRoleKeyChangeAlreadyPending" .= key]

validateRegisterRole
    :: Monad m
    => Validation m
    -> ForRole
    -> Change RegisterRoleKey (OpI ())
    -> Validate RegisterRoleFailure m Validated
validateRegisterRole
    validation@Validation{githubRepositoryRole}
    forRole
    change@(Change (Key k) _) = do
        when (forUser forRole)
            $ keyAlreadyPendingFailure
                validation
                RegisterRoleKeyChangeAlreadyPending
                k
                requestZooGetRegisterRoleKey
        mapFailure RegisterRoleKeyFailure $ insertValidation validation change
        let RegisterRoleKey (Platform platform) repository username = k
        when (platform /= "github")
            $ notValidated
            $ RegisterRolePlatformNotSupported platform
        validationRes <- lift $ githubRepositoryRole username repository
        case validationRes of
            Just failure -> notValidated $ RoleNotPresentOnPlatform failure
            Nothing -> pure Validated

data UnregisterRoleFailure
    = UnregisterRoleKeyFailure KeyFailure
    | UnregisterRoleKeyChangeAlreadyPending RegisterRoleKey
    | UnregisterRolePlatformNotSupported String
    | UnregisterRoleRoleIsStillValidInGithub
    | UnregisterRoleGithubGetError String
    deriving (Show, Eq)

instance Monad m => ToJSON m UnregisterRoleFailure where
    toJSON = \case
        UnregisterRoleKeyFailure keyFailure ->
            object ["unregisterRoleKeyFailure" .= keyFailure]
        UnregisterRoleKeyChangeAlreadyPending key ->
            object ["unregisterRoleKeyChangeAlreadyPending" .= key]
        UnregisterRolePlatformNotSupported platform ->
            object ["unregisterRolePlatformNotSupported" .= platform]
        UnregisterRoleRoleIsStillValidInGithub ->
            object ["unregisterRoleRoleIsStillValidInGithub" .= ()]
        UnregisterRoleGithubGetError err ->
            object ["unregisterRoleGithubGetError" .= err]

validateUnregisterRole
    :: Monad m
    => Validation m
    -> ForRole
    -> Change RegisterRoleKey (OpD ())
    -> Validate UnregisterRoleFailure m Validated
validateUnregisterRole
    validation@Validation{githubRepositoryRole}
    forRole
    change@(Change (Key k) _) = do
        when (forUser forRole)
            $ keyAlreadyPendingFailure
                validation
                UnregisterRoleKeyChangeAlreadyPending
                k
                requestZooGetRegisterRoleKey
        mapFailure UnregisterRoleKeyFailure
            $ deleteValidation validation change
        let RegisterRoleKey (Platform platform) repository username = k
        when (platform /= "github")
            $ notValidated
            $ UnregisterRolePlatformNotSupported platform
        validationRes <- lift $ githubRepositoryRole username repository
        case validationRes of
            Just NoRoleEntryInCodeowners -> pure Validated
            Just NoUsersAssignedToRoleInCodeowners -> pure Validated
            Just NoUserInCodeowners -> pure Validated
            Just (GithubGetError err) ->
                notValidated $ UnregisterRoleGithubGetError (show err)
            Nothing -> notValidated UnregisterRoleRoleIsStillValidInGithub
