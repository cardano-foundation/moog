{-# LANGUAGE ApplicativeDo #-}

module Core.Types.MPFS
    ( MPFSClient (..)
    , newClient
    , mpfsClientOption
    )
where

import Core.Types.Wallet (Wallet)
import Network.HTTP.Client (defaultManagerSettings, newManager)
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

newMPFSClient :: Parser (String, IfToWait)
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
    pure (host, wait)

newClient :: (String, IfToWait) -> IO MPFSClient
newClient (host, wait) = do
    baseUrl <- parseBaseUrl host
    manager <-
        newManager
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
