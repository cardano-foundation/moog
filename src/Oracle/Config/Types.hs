module Oracle.Config.Types
    ( Config (..)
    , ConfigKey (..)
    , SetConfigChange
    , UpdateConfigChange
    , currentProtocolVersion
    , mkCurrentConfig
    , ProtocolVersion (..)
    , ProtocolFailure (..)
    )
where

import Control.Applicative (Alternative, (<|>))
import Core.Types.Basic (Owner)
import Core.Types.Change (Change)
import Core.Types.Operation (Op (OpI, OpU))
import Lib.JSON.Canonical.Extra
    ( intJSON
    , object
    , stringJSON
    , withObject
    , (.:)
    , (.=)
    )
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , Int54
    , JSValue (JSString)
    , ReportSchemaErrors
    , ToJSON (..)
    , expectedButGotValue
    , toJSString
    )

newtype ProtocolVersion = ProtocolVersion Int
    deriving (Show, Eq, Ord)

instance Monad m => ToJSON m ProtocolVersion where
    toJSON (ProtocolVersion v) = intJSON v
instance (Alternative m, ReportSchemaErrors m) => FromJSON m ProtocolVersion where
    fromJSON v = ProtocolVersion . fromIntegral @Int54 <$> fromJSON v

currentProtocolVersion :: ProtocolVersion
currentProtocolVersion = ProtocolVersion 0

data Config = Config
    { configAgent :: Owner
    , configTestRun :: TestRunValidationConfig
    , configProtocolVersion :: ProtocolVersion
    }
    deriving (Show, Eq)

mkCurrentConfig :: Owner -> TestRunValidationConfig -> Config
mkCurrentConfig agent testRun =
    Config
        { configAgent = agent
        , configTestRun = testRun
        , configProtocolVersion = currentProtocolVersion
        }

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
    toJSON (Config agent testRun protocolVersion) =
        object
            [ "agent" .= agent
            , "testRun" .= testRun
            , "protocolVersion" .= protocolVersion
            ]

instance (Alternative m, ReportSchemaErrors m) => FromJSON m Config where
    fromJSON = withObject "Config" $ \o -> do
        Config
            <$> o .: "agent"
            <*> o .: "testRun"
            <*> ( (o .: "protocolVersion" >>= fromJSON)
                    <|> pure (ProtocolVersion 0)
                )

type SetConfigChange = Change ConfigKey (OpI Config)
type UpdateConfigChange = Change ConfigKey (OpU Config Config)

data ProtocolFailure
    = ConfigNotAvailable
    | UnsupportedProtocolVersion ProtocolVersion ProtocolVersion
    deriving (Show, Eq)

instance Monad m => ToJSON m ProtocolFailure where
    toJSON ConfigNotAvailable =
        stringJSON "configNotAvailable"
    toJSON (UnsupportedProtocolVersion found expected) =
        object
            [ (,) "unsupportedProtocolVersion"
                $ object
                    [ "found" .= found
                    , "expected" .= expected
                    ]
            ]
