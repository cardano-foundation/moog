{-# LANGUAGE DeriveGeneric #-}

module Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    ) where

import GHC.Generics (Generic)
import Lib.JSON.Canonical.Extra
import Text.JSON.Canonical
    ( FromJSON (..)
    , Int54
    , ReportSchemaErrors
    , ToJSON (..)
    )

data TestRunValidationConfig = TestRunValidationConfig
    { maxDuration :: Int
    , minDuration :: Int
    }
    deriving (Show, Eq, Generic)

instance Monad m => ToJSON m TestRunValidationConfig where
    toJSON (TestRunValidationConfig maxDur minDur) =
        object
            [ "maxDuration" .= fromIntegral @_ @Int54 maxDur
            , "minDuration" .= fromIntegral @_ @Int54 minDur
            ]

instance ReportSchemaErrors m => FromJSON m TestRunValidationConfig where
    fromJSON = withObject "TestRunValidationConfig" $ \o ->
        TestRunValidationConfig
            <$> (o .: "maxDuration" >>= pure . fromIntegral @Int54)
            <*> (o .: "minDuration" >>= pure . fromIntegral @Int54)
