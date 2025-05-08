module Anti.Main (main) where

import Anti.Cli (anti)
import Anti.Options (parseArgs)
import Anti.Types (Host (..), Options (Options), Port (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client
    ( BaseUrl
        ( BaseUrl
        , baseUrlHost
        , baseUrlPath
        , baseUrlPort
        , baseUrlScheme
        )
    , Scheme (Http)
    , mkClientEnv
    , runClientM, ClientError
    )
import System.Environment (getArgs)
import Data.Aeson (Value)

main :: IO (Either ClientError Value)
main = do
    args <- getArgs
    Options tokenId (Host host) (Port port) command <- parseArgs args
    manger <- newManager defaultManagerSettings
    let baseUrl =
            BaseUrl
                { baseUrlScheme = Http
                , baseUrlHost = host
                , baseUrlPort = port
                , baseUrlPath = ""
                }
        clientEnv = mkClientEnv manger baseUrl
    runClientM (anti tokenId command) clientEnv
