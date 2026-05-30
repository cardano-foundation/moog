{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Direct Antithesis read-API client for the long-running agent.
module User.Agent.Antithesis.Client
    ( AntithesisApiConfig (..)
    , AntithesisApiKey (..)
    , AntithesisApiUrl (..)
    , AntithesisApiError (..)
    , deriveAntithesisApiUrl
    , listRunsPage
    , listAllRuns
    )
where

import Control.Exception (SomeException, try)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAuthorization, statusCode)
import Proxy.Antithesis.Api
    ( JsonClient (..)
    , antithesisProxyJsonClient
    )
import Servant.Client
    ( ClientEnv
    , ClientError (..)
    , ClientM
    , defaultMakeClientRequest
    , makeClientRequest
    , mkClientEnv
    , parseBaseUrl
    , responseBody
    , responseStatusCode
    , runClientM
    )
import User.Agent.Antithesis.State
    ( AntithesisRun
    , RunsPage (..)
    , parseRunsPage
    )
import User.Agent.PushTest (LaunchUrl (..))

newtype AntithesisApiUrl = AntithesisApiUrl {unAntithesisApiUrl :: String}
    deriving (Eq, Show)

newtype AntithesisApiKey = AntithesisApiKey {unAntithesisApiKey :: ByteString}
    deriving (Eq, Show)

data AntithesisApiConfig = AntithesisApiConfig
    { antithesisApiUrl :: AntithesisApiUrl
    , antithesisApiKey :: AntithesisApiKey
    }
    deriving (Eq, Show)

data AntithesisApiError
    = AntithesisApiSetupError Text
    | AntithesisApiHttpError Int Text
    | AntithesisApiInvalidJson Text
    | AntithesisApiTransportError Text
    deriving (Eq, Show)

deriveAntithesisApiUrl :: LaunchUrl -> AntithesisApiUrl
deriveAntithesisApiUrl (LaunchUrl url) =
    AntithesisApiUrl $
        stripTrailingSlash $
            T.unpack $
                if T.null suffix
                    then raw
                    else base
  where
    raw = T.pack url
    (base, suffix) = T.breakOn "/api/v1/launch/" raw

listRunsPage
    :: AntithesisApiConfig
    -> Maybe Int
    -> Maybe Text
    -> IO (Either AntithesisApiError RunsPage)
listRunsPage config limit cursor =
    withApiEnv config $ \env ->
        runJsonRequest env $
            listRuns antithesisProxyJsonClient limit cursor

listAllRuns :: AntithesisApiConfig -> IO (Either AntithesisApiError [AntithesisRun])
listAllRuns config =
    withApiEnv config $ \env ->
        go env Nothing []
  where
    go env cursor acc = do
        result <-
            runJsonRequest env $
                listRuns antithesisProxyJsonClient (Just 100) cursor
        case result of
            Left err -> pure $ Left err
            Right RunsPage{runsPageRuns, runsPageNextCursor} ->
                case runsPageNextCursor of
                    Nothing -> pure $ Right $ acc <> runsPageRuns
                    Just nextCursor -> go env (Just nextCursor) (acc <> runsPageRuns)

withApiEnv
    :: AntithesisApiConfig
    -> (ClientEnv -> IO (Either AntithesisApiError a))
    -> IO (Either AntithesisApiError a)
withApiEnv config action = do
    envResult <- try @SomeException $ makeApiEnv config
    case envResult of
        Left err ->
            pure $
                Left $
                    AntithesisApiSetupError $
                        T.pack $
                            show err
        Right env -> action env

makeApiEnv :: AntithesisApiConfig -> IO ClientEnv
makeApiEnv
    AntithesisApiConfig
        { antithesisApiUrl = AntithesisApiUrl url
        , antithesisApiKey = AntithesisApiKey key
        } = do
        manager <- newManager tlsManagerSettings
        baseUrl <- parseBaseUrl url
        pure $ withBearer key $ mkClientEnv manager baseUrl

withBearer :: ByteString -> ClientEnv -> ClientEnv
withBearer key env =
    let injecting baseUrl req = do
            baseRequest <- defaultMakeClientRequest baseUrl req
            let bearer = (hAuthorization, "Bearer " <> key)
                headers =
                    bearer
                        : filter
                            ((/= hAuthorization) . fst)
                            (HC.requestHeaders baseRequest)
            pure $ baseRequest{HC.requestHeaders = headers}
     in env{makeClientRequest = injecting}

runJsonRequest
    :: ClientEnv
    -> ClientM Aeson.Value
    -> IO (Either AntithesisApiError RunsPage)
runJsonRequest env action = do
    result <- try @SomeException $ runClientM action env
    pure $ case result of
        Left err ->
            Left $ AntithesisApiTransportError $ T.pack $ show err
        Right (Left err) ->
            Left $ fromClientError err
        Right (Right value) ->
            case parseRunsPage value of
                Left err -> Left $ AntithesisApiInvalidJson $ T.pack err
                Right page -> Right page

fromClientError :: ClientError -> AntithesisApiError
fromClientError = \case
    FailureResponse _ resp ->
        AntithesisApiHttpError
            (statusCode $ responseStatusCode resp)
            (T.pack $ show $ responseBody resp)
    DecodeFailure msg _ -> AntithesisApiInvalidJson msg
    UnsupportedContentType _ _ ->
        AntithesisApiInvalidJson "unsupported content type from Antithesis API"
    InvalidContentTypeHeader _ ->
        AntithesisApiInvalidJson "invalid content type header from Antithesis API"
    ConnectionError err ->
        AntithesisApiTransportError $ T.pack $ show err

stripTrailingSlash :: String -> String
stripTrailingSlash =
    reverse . dropWhile (== '/') . reverse
