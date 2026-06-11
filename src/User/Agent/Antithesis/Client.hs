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
    , listAllProperties
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
    , PropertiesPage (..)
    , RunProperty
    , RunsPage (..)
    , parsePropertiesPage
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
        runJsonRequest parseRunsPage env $
            listRuns antithesisProxyJsonClient limit cursor

listAllRuns :: AntithesisApiConfig -> IO (Either AntithesisApiError [AntithesisRun])
listAllRuns config =
    withApiEnv config $ \env ->
        go env Nothing []
  where
    go env cursor acc = do
        result <-
            runJsonRequest parseRunsPage env $
                listRuns antithesisProxyJsonClient (Just 100) cursor
        case result of
            Left err -> pure $ Left err
            Right RunsPage{runsPageRuns, runsPageNextCursor} ->
                let acc' = acc <> runsPageRuns
                 in case runsPageNextCursor of
                        Nothing -> pure $ Right acc'
                        -- Stop if the cursor fails to advance. A
                        -- non-terminating upstream cursor (e.g. an ignored
                        -- pagination param) must never accumulate pages until
                        -- the agent OOMs; bail instead of looping forever.
                        Just nextCursor
                            | Just nextCursor == cursor -> pure $ Right acc'
                            | otherwise -> go env (Just nextCursor) acc'

-- | Fetch all properties of a run, following @after@/@next_cursor@ to the
-- end (50–100 per page upstream) so later pages of assertions are not
-- missed. Same stop-if-cursor-unchanged guard as 'listAllRuns'.
listAllProperties
    :: AntithesisApiConfig
    -> Text
    -> IO (Either AntithesisApiError [RunProperty])
listAllProperties config runId =
    withApiEnv config $ \env ->
        go env Nothing []
  where
    go env cursor acc = do
        result <-
            runJsonRequest parsePropertiesPage env $
                getProperties antithesisProxyJsonClient runId cursor
        case result of
            Left err -> pure $ Left err
            Right PropertiesPage{propsPageData, propsPageNextCursor} ->
                let acc' = acc <> propsPageData
                 in case propsPageNextCursor of
                        Nothing -> pure $ Right acc'
                        Just nextCursor
                            | Just nextCursor == cursor -> pure $ Right acc'
                            | otherwise -> go env (Just nextCursor) acc'

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
    :: (Aeson.Value -> Either String a)
    -> ClientEnv
    -> ClientM Aeson.Value
    -> IO (Either AntithesisApiError a)
runJsonRequest parse env action = do
    result <- try @SomeException $ runClientM action env
    pure $ case result of
        Left err ->
            Left $ AntithesisApiTransportError $ T.pack $ show err
        Right (Left err) ->
            Left $ fromClientError err
        Right (Right value) ->
            case parse value of
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
