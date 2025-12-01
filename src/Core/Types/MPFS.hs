{-# LANGUAGE ApplicativeDo #-}

module Core.Types.MPFS
    ( MPFSClient (..)
    , newClient
    , mpfsClientOption
    )
where

import Core.Types.Wallet (Wallet)
import Network.HTTP.Client
    ( ManagerSettings (..)
    , defaultManagerSettings
    , newManager
    , responseTimeoutMicro
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OptEnvConf
import Servant.Client
    ( BaseUrl (..)
    , ClientM
    , Scheme (..)
    , mkClientEnv
    , parseBaseUrl
    , runClientM
    )
import Submitting
    ( IfToWait (..)
    , Submission
    , Submitting (..)
    , signAndSubmitMPFS
    )

data MPFSClient = MPFSClient
    { runMPFS :: forall a. ClientM a -> IO a
    , submitTx :: Wallet -> Submission ClientM
    }

newMPFSClient :: Parser (String, IfToWait, Int)
newMPFSClient = do
    host <-
        setting
            [ env "MOOG_MPFS_HOST"
            , metavar "HOST"
            , help "The host of the MPFS server"
            , reader str
            ]
    wait <-
        setting
            [ env "MOOG_WAIT"
            , metavar "WAIT"
            , help "Whether to wait for the transaction to be included in a block"
            , reader $ Wait <$> auto
            , value NoWait
            ]
    timeoutSeconds <-
        setting
            [ metavar "SECONDS"
            , help "Timeout in seconds for MPFS requests"
            , env "MOOG_MPFS_TIMEOUT_SECONDS"
            , reader auto
            , value 120
            ]
    pure (host, wait, timeoutSeconds)

timeout :: Int -> ManagerSettings -> ManagerSettings
timeout tOut r =
    r
        { managerResponseTimeout =
            responseTimeoutMicro $ tOut * 1000000
        }

newClient :: (String, IfToWait, Int) -> IO MPFSClient
newClient (host, wait, tOut) = do
    baseUrl <- parseBaseUrl host
    manager <-
        newManager
            $ timeout tOut
            $ if baseUrlScheme baseUrl == Https
                then tlsManagerSettings
                else defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl
    let runMPFS :: forall a. ClientM a -> IO a
        runMPFS c = do
            e <- runClientM c clientEnv
            case e of
                Left err -> error $ "Client error: " ++ show err
                Right r -> return r
        submit = signAndSubmitMPFS Submitting{ifToWait = wait, runClient = runMPFS}
    return $ MPFSClient runMPFS submit

mpfsClientOption :: Parser MPFSClient
mpfsClientOption = mapIO newClient newMPFSClient
