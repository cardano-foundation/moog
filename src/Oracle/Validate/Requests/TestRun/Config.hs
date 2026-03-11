{-# LANGUAGE DeriveGeneric #-}

module Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    , testRunValidationConfigFromV0
    ) where

import Control.Applicative (Alternative)
import Core.Types.Duration (Duration, durationFromV0)
import GHC.Generics (Generic)
import Lib.JSON.Canonical.Extra
    ( getField
    , object
    , withObject
    , (.=)
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue
    , ReportSchemaErrors
    , ToJSON (..)
    )

data TestRunValidationConfig = TestRunValidationConfig
    { maxDuration :: Duration
    , minDuration :: Duration
    }
    deriving (Show, Eq, Generic)

instance Monad m => ToJSON m TestRunValidationConfig where
    toJSON (TestRunValidationConfig maxDur minDur) =
        object
            [ "maxDuration" .= maxDur
            , "minDuration" .= minDur
            ]

instance
    (Alternative m, ReportSchemaErrors m)
    => FromJSON m TestRunValidationConfig
    where
    fromJSON = withObject "TestRunValidationConfig" $ \o ->
        TestRunValidationConfig
            <$> (getField "maxDuration" o >>= fromJSON)
            <*> (getField "minDuration" o >>= fromJSON)

-- | Parse a v0 wire format 'TestRunValidationConfig'
-- where durations are plain numbers of hours.
testRunValidationConfigFromV0
    :: ReportSchemaErrors m
    => JSValue
    -> m TestRunValidationConfig
testRunValidationConfigFromV0 =
    withObject "TestRunValidationConfig" $ \o ->
        TestRunValidationConfig
            <$> ( getField "maxDuration" o
                    >>= durationFromV0
                )
            <*> ( getField "minDuration" o
                    >>= durationFromV0
                )
