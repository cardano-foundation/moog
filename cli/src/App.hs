module App (client) where

import Cli (cmd)
import Control.Exception (catch)
import Core.Types
    ( TokenId (..)
    )
import Lib.Box (Box (..))
import Network.HTTP.Client
    ( ManagerSettings (..)
    , Request (requestBody)
    , RequestBody (..)
    , defaultManagerSettings
    , newManager
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options (Options (..), parseArgs)
import Servant.Client
    ( BaseUrl (..)
    , ClientError
    , ClientM
    , Scheme (..)
    , mkClientEnv
    , parseBaseUrl
    , runClientM
    )
import Submitting (IfToWait (..), Submitting (..), readWallet)
import System.Environment (getArgs, getEnv, lookupEnv)
import Text.JSON.Canonical (JSValue, ToJSON (..))

client
    :: IO (Box Options, FilePath, String, Either ClientError JSValue)
client = do
    args <- getArgs
    o@(Box (Options command)) <- parseArgs args
    mpfs_host <- getEnv "ANTI_MPFS_HOST"
    mtk <- fmap TokenId <$> lookupEnv "ANTI_TOKEN_ID"
    baseUrl <- parseBaseUrl mpfs_host
    walletFile <- getEnv "ANTI_WALLET_FILE"
    mWallet <-
        (Right <$> readWallet walletFile)
            `catch` \(_ :: IOError) -> return $ Left walletFile
    manger <-
        newManager
            $ if baseUrlScheme baseUrl == Https
                then tlsManagerSettings
                else defaultManagerSettings
    let clientEnv = mkClientEnv manger baseUrl
    eiftw <- lookupEnv "ANTI_WAIT"
    let iftw = case eiftw of
            Just s -> Wait (read s)
            Nothing -> NoWait
        runClient :: forall a. ClientM a -> IO a
        runClient c = do
            e <- runClientM c clientEnv
            case e of
                Left err -> error $ "Client error: " ++ show err
                Right r -> return r
    let sbmt = Submitting{ifToWait = iftw, runClient}
    (o,walletFile,mpfs_host,)
        <$> runClientM (cmd sbmt mWallet mtk command >>= toJSON) clientEnv

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
