{-# LANGUAGE TupleSections #-}

module Anti.Main (main) where

import Anti.Cli (anti)
import Anti.Options (parseArgs)
import Anti.Types (Host (..), Options (Options), Port (..))
import Data.Aeson (Value)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client
    ( BaseUrl
        ( BaseUrl
        , baseUrlHost
        , baseUrlPath
        , baseUrlPort
        , baseUrlScheme
        )
    , ClientError
    , Scheme (Http)
    , mkClientEnv
    , runClientM
    )
import System.Environment (getArgs)

main :: IO (Options, Either ClientError Value)
main = do
    args <- getArgs
    o@(Options tokenId (Host host) (Port port) command) <- parseArgs args
    manger <- newManager defaultManagerSettings
    let baseUrl =
            BaseUrl
                { baseUrlScheme = Http
                , baseUrlHost = host
                , baseUrlPort = port
                , baseUrlPath = ""
                }
        clientEnv = mkClientEnv manger baseUrl
    (o,) <$> runClientM (anti tokenId command) clientEnv
