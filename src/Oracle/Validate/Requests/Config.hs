module Oracle.Validate.Requests.Config
    ( validateInsertConfig
    , ConfigFailure (..)
    , validateUpdateConfig
    )
where

import Control.Monad (when)
import Core.Types.Basic (Owner)
import Core.Types.Change (Change (..))
import Core.Types.Duration (Duration)
import Core.Types.Operation (Op (..), Operation (..))
import Effects
    ( Effects
    , KeyFailure
    , insertValidation
    , updateValidation
    )
import Lib.JSON.Canonical.Extra (object, (.=))
import Oracle.Config.Types
    ( Config (Config, configTestRun)
    , ConfigKey
    )
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Types
    ( Validate
    , Validated (..)
    , mapFailure
    , notValidated
    )
import Text.JSON.Canonical (ToJSON (..))

data ConfigFailure
    = ConfigureKeyValidationFailure KeyFailure
    | ConfigureNotFromOracle Owner
    | ConfigureMinLessThanZero Duration
    | ConfigureMaxLessThanMin Duration Duration
    deriving (Show, Eq)

instance Monad m => ToJSON m ConfigFailure where
    toJSON (ConfigureKeyValidationFailure keyFailure) =
        object ["configureKeyValidationFailure" .= keyFailure]
    toJSON (ConfigureNotFromOracle owner) =
        object ["configureNotFromOracle" .= show owner]
    toJSON (ConfigureMinLessThanZero minD) =
        object ["configureMinLessThanZero" .= minD]
    toJSON (ConfigureMaxLessThanMin maxD minD) =
        object
            [ (,) "configureMaxLessThanMin"
                $ object
                    [ "max" .= maxD
                    , "min" .= minD
                    ]
            ]

commonValidation
    :: Monad m
    => Owner
    -> Owner
    -> TestRunValidationConfig
    -> Validate ConfigFailure m Validated
commonValidation oracleOwner submitterOwner configTestRun = do
    when (submitterOwner /= oracleOwner)
        $ notValidated
        $ ConfigureNotFromOracle submitterOwner
    let minD = minDuration configTestRun
        maxD = maxDuration configTestRun
    when (minD <= mempty)
        $ notValidated
        $ ConfigureMinLessThanZero minD
    when (maxD < minD)
        $ notValidated
        $ ConfigureMaxLessThanMin maxD minD
    pure Validated

validateInsertConfig
    :: Monad m
    => Effects m
    -> Owner
    -- ^ Oracle
    -> Owner
    -- ^ Submitter
    -> Change ConfigKey (OpI Config)
    -> Validate ConfigFailure m Validated
validateInsertConfig
    validation
    oracleOwner
    submitterOwner
    change@(Change _ (Insert Config{configTestRun})) = do
        mapFailure ConfigureKeyValidationFailure
            $ insertValidation validation change
        commonValidation
            oracleOwner
            submitterOwner
            configTestRun

validateUpdateConfig
    :: Monad m
    => Effects m
    -> Owner
    -- ^ Oracle
    -> Owner
    -- ^ Submitter
    -> Change ConfigKey (OpU Config Config)
    -> Validate ConfigFailure m Validated
validateUpdateConfig
    validation
    oracleOwner
    submitterOwner
    change@(Change _ (Update _ Config{configTestRun})) = do
        mapFailure ConfigureKeyValidationFailure
            $ updateValidation validation change
        commonValidation
            oracleOwner
            submitterOwner
            configTestRun
