{-# LANGUAGE DeriveGeneric #-}

module Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    ) where

import Control.Applicative (Alternative)
import Core.Types.Duration (Duration)
import GHC.Generics (Generic)
import Lib.JSON.Canonical.Extra
import Text.JSON.Canonical
    ( FromJSON (..)
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
            <$> (o .: "maxDuration" >>= fromJSON)
            <*> (o .: "minDuration" >>= fromJSON)
