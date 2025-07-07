{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Logic
    ( ValidationResult (..)
    , validateRequest
    ) where

import Core.Types (RequestRefId)
import Lib.JSON
    ( stringJSON
    )
import Oracle.Types (Request (..), RequestZoo (..))
import Servant.Client (ClientM)
import Text.JSON.Canonical.Class (ToJSON (..))

data ValidationResult
    = Validated
    | NotValidated
    | CannotValidate
    | NotEvaluated
    deriving (Eq, Show)

instance Monad m => ToJSON m ValidationResult where
    toJSON = \case
        Validated -> stringJSON "validated"
        NotValidated -> stringJSON "not validated"
        CannotValidate -> stringJSON "cannot validate"
        NotEvaluated -> stringJSON "not evaluated"

validateRequest
    :: RequestZoo
    -> ClientM (RequestRefId, ValidationResult)
validateRequest (RegisterUserRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (UnregisterUserRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (RegisterRoleRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (UnregisterRoleRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (CreateTestRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (RejectRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (AcceptRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (FinishedRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
