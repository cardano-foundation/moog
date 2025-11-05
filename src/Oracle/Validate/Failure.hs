module Oracle.Validate.Failure
    ( RequestValidationFailure (..)
    ) where

import Core.Types.Operation (Op (..))
import Lib.JSON.Canonical.Extra (object, (.=))
import Oracle.Types (Request)
import Oracle.Validate.Requests.Config (ConfigFailure)
import Oracle.Validate.Requests.ManageWhiteList
    ( UpdateWhiteListFailure
    )
import Oracle.Validate.Requests.RegisterRole
    ( RegisterRoleFailure (..)
    , UnregisterRoleFailure (..)
    )
import Oracle.Validate.Requests.RegisterUser
    ( RegisterUserFailure (..)
    , UnregisterUserFailure (..)
    )
import Oracle.Validate.Requests.TestRun.Create
    ( CreateTestRunFailure (..)
    )
import Oracle.Validate.Requests.TestRun.Update
    ( UpdateTestRunFailure (..)
    )
import Text.JSON.Canonical (ToJSON (..))
import Text.JSON.Canonical.Types (JSValue)

data RequestValidationFailure
    = RegisterUserFailure RegisterUserFailure
    | UnregisterUserFailure UnregisterUserFailure
    | RegisterRoleFailure RegisterRoleFailure
    | UnregisterRoleFailure UnregisterRoleFailure
    | CreateTestRunFailure CreateTestRunFailure
    | UpdateTestRunFailure UpdateTestRunFailure
    | WhiteListFailure UpdateWhiteListFailure
    | ConfigFailure ConfigFailure
    | RequestValidationConfigNotAvailable
    | UnknownInsertValidationFailure (Request JSValue ('OpI JSValue))
    | UnknownDeleteValidationFailure (Request JSValue ('OpD JSValue))
    | UnknownUpdateValidationFailure
        (Request JSValue ('OpU JSValue JSValue))
    deriving (Eq, Show)

instance Monad m => ToJSON m RequestValidationFailure where
    toJSON = \case
        RegisterUserFailure failure ->
            object ["RegisterUserFailure" .= failure]
        UnregisterUserFailure failure ->
            object ["UnregisterUserFailure" .= failure]
        RegisterRoleFailure failure ->
            object ["RegisterRoleFailure" .= failure]
        UnregisterRoleFailure failure ->
            object ["UnregisterRoleFailure" .= failure]
        CreateTestRunFailure failure ->
            object ["CreateTestRunFailure" .= failure]
        UpdateTestRunFailure failure ->
            object ["UpdateTestRunFailure" .= failure]
        WhiteListFailure failure ->
            object ["WhiteListFailure" .= failure]
        ConfigFailure failure ->
            object ["ConfigFailure" .= failure]
        RequestValidationConfigNotAvailable ->
            toJSON ("Token configuration is not available yet" :: String)
        UnknownInsertValidationFailure value ->
            object ["UnknownInsertValidationFailure" .= value]
        UnknownDeleteValidationFailure value ->
            object ["UnknownDeleteValidationFailure" .= value]
        UnknownUpdateValidationFailure value ->
            object ["UnknownUpdateValidationFailure" .= value]
