{-# LANGUAGE UndecidableInstances #-}

module App
    ( client
    , Result (..)
    ) where

import Cli (cmd)
import Control.Exception (SomeException (SomeException), catch, try)
import Lib.Box (Box (..))
import Network.HTTP.Client
    ( ManagerSettings (..)
    , Request (requestBody)
    , RequestBody (..)
    )
import Options (Options (..), parseArgs)
import Paths_moog (version)
import Text.JSON.Canonical (JSValue, ToJSON (..))

data Result
    = Success Bool JSValue
    | Exceptional Bool SomeException
    | Help
    deriving (Show)

client
    :: IO Result
client = do
    fc <- try $ parseArgs version
    case fc of
        Left (SomeException _) -> return Help
        Right (Box (Options raw command)) -> do
            let action = Success raw <$> (cmd command >>= toJSON)
            action `catch` (return . Exceptional raw)

_logRequests :: ManagerSettings -> ManagerSettings
_logRequests settings =
    settings
        { managerModifyRequest =
            \req -> do
                print req
                case requestBody req of
                    RequestBodyLBS body -> do
                        putStrLn $ "Request Body: " ++ show body
                    _ -> return ()
                return req
        }
