module Oracle.Config.Types
    ( Config (..)
    , ConfigKey (..)
    , SetConfigChange
    , UpdateConfigChange
    )
where

import Control.Applicative (Alternative)
import Core.Types.Basic (Owner)
import Core.Types.Change (Change)
import Core.Types.Operation (Op (OpI, OpU))
import Lib.JSON.Canonical.Extra (object, withObject, (.:), (.=))
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (JSString)
    , ReportSchemaErrors
    , ToJSON (..)
    , expectedButGotValue
    , toJSString
    )

data Config = Config
    { configAgent :: Owner
    -- ^ Agent PKH
    , configTestRun :: TestRunValidationConfig
    }
    deriving (Show, Eq)

data ConfigKey = ConfigKey
    deriving (Show, Eq)

instance Monad m => ToJSON m ConfigKey where
    toJSON _ = object ["type" .= ("config" :: String)]

instance ReportSchemaErrors m => FromJSON m ConfigKey where
    fromJSON = withObject "ConfigKey" $ \o -> do
        type' <- o .: "type"
        if type' /= ("config" :: String)
            then
                expectedButGotValue
                    "config"
                    $ JSString
                    $ toJSString type'
            else pure ConfigKey

instance Monad m => ToJSON m Config where
    toJSON (Config agent testRun) =
        object
            [ "agent" .= agent
            , "testRun" .= testRun
            ]

instance (Alternative m, ReportSchemaErrors m) => FromJSON m Config where
    fromJSON = withObject "Config" $ \o -> do
        Config
            <$> o .: "agent"
            <*> o .: "testRun"

type SetConfigChange = Change ConfigKey (OpI Config)
type UpdateConfigChange = Change ConfigKey (OpU Config Config)
