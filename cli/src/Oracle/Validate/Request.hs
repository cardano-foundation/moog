module Oracle.Validate.Request
    ( validateRequest
    ) where

import Core.Types.Basic (Owner)
import Effects (Effects (..))
import Oracle.Config.Types (Config (..))
import Oracle.Types
    ( Request (..)
    , RequestZoo (..)
    )
import Oracle.Validate.Failure (RequestValidationFailure (..))
import Oracle.Validate.Requests.Config
    ( validateInsertConfig
    , validateUpdateConfig
    )
import Oracle.Validate.Requests.ManageWhiteList
    ( validateAddWhiteListed
    , validateRemoveWhiteListed
    )
import Oracle.Validate.Requests.RegisterRole
    ( validateRegisterRole
    , validateUnregisterRole
    )
import Oracle.Validate.Requests.RegisterUser
    ( validateRegisterUser
    , validateUnregisterUser
    )
import Oracle.Validate.Requests.TestRun.Create (validateCreateTestRun)
import Oracle.Validate.Requests.TestRun.Update
    ( validateToDoneUpdate
    , validateToRunningUpdate
    )
import Oracle.Validate.Types
    ( ForRole (..)
    , Validate
    , Validated
    , mapFailure
    , notValidated
    )

validateRequest
    :: Monad m
    => Owner
    -> Maybe Config
    -> Effects m
    -> RequestZoo
    -> Validate RequestValidationFailure m Validated
validateRequest _ _ validation (RegisterUserRequest (Request _ _ change)) =
    mapFailure RegisterUserFailure
        $ validateRegisterUser validation ForOracle change
validateRequest _ _ validation (UnregisterUserRequest (Request _ _ change)) =
    mapFailure UnregisterUserFailure
        $ validateUnregisterUser validation ForOracle change
validateRequest _ _ validation (RegisterRoleRequest (Request _ _ change)) =
    mapFailure RegisterRoleFailure
        $ validateRegisterRole validation ForOracle change
validateRequest _ _ validation (UnregisterRoleRequest (Request _ _ change)) =
    mapFailure UnregisterRoleFailure
        $ validateUnregisterRole validation ForOracle change
validateRequest _ (Just Config{configTestRun}) validation (CreateTestRequest (Request _ _ change)) =
    mapFailure CreateTestRunFailure
        $ validateCreateTestRun configTestRun validation ForOracle change
validateRequest _ Nothing _ CreateTestRequest{} =
    notValidated RequestValidationConfigNotAvailable
validateRequest _ (Just Config{configAgent}) validation (RejectRequest (Request _ requester change)) =
    mapFailure UpdateTestRunFailure
        $ validateToDoneUpdate validation ForOracle configAgent requester change
validateRequest _ Nothing _ RejectRequest{} =
    notValidated RequestValidationConfigNotAvailable
validateRequest _ (Just Config{configAgent}) validation (AcceptRequest (Request _ requester change)) =
    mapFailure UpdateTestRunFailure
        $ validateToRunningUpdate
            validation
            ForOracle
            configAgent
            requester
            change
validateRequest _ Nothing _ AcceptRequest{} =
    notValidated RequestValidationConfigNotAvailable
validateRequest _ (Just Config{configAgent}) validation (FinishedRequest (Request _ requester change)) =
    mapFailure UpdateTestRunFailure
        $ validateToDoneUpdate validation ForOracle configAgent requester change
validateRequest _ Nothing _ FinishedRequest{} =
    notValidated RequestValidationConfigNotAvailable
validateRequest _ (Just Config{configAgent}) validation (WhiteListRequest (Request _ requester change)) =
    mapFailure WhiteListFailure
        $ validateAddWhiteListed validation configAgent requester change
validateRequest _ Nothing _ WhiteListRequest{} =
    notValidated RequestValidationConfigNotAvailable
validateRequest _ (Just Config{configAgent}) validation (BlackListRequest (Request _ requester change)) =
    mapFailure WhiteListFailure
        $ validateRemoveWhiteListed validation configAgent requester change
validateRequest _ Nothing _ BlackListRequest{} =
    notValidated RequestValidationConfigNotAvailable
validateRequest oracle _ validation (InsertConfigRequest (Request _ requester change)) =
    mapFailure ConfigFailure
        $ validateInsertConfig validation oracle requester change
validateRequest oracle _ validation (UpdateConfigRequest (Request _ requester change)) =
    mapFailure ConfigFailure
        $ validateUpdateConfig validation oracle requester change
validateRequest _ _ _ (UnknownInsertRequest request) =
    notValidated
        $ UnknownInsertValidationFailure request
validateRequest _ _ _ (UnknownDeleteRequest request) =
    notValidated
        $ UnknownDeleteValidationFailure request
validateRequest _ _ _ (UnknownUpdateRequest request) =
    notValidated
        $ UnknownUpdateValidationFailure request
