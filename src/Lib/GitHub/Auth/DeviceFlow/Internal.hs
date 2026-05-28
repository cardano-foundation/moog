module Lib.GitHub.Auth.DeviceFlow.Internal
    ( ClientId (..)
    , Scope (..)
    , OAuthToken (..)
    , DeviceCodeResponse (..)
    , DeviceFlowError (..)
    , DeviceFlowEndpoints (..)
    , githubDeviceFlowEndpoints
    , runDeviceFlowWith
    )
where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
    ( FromJSON (parseJSON)
    , eitherDecode
    , withObject
    , (.:)
    )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Client
    ( HttpException
    , Manager
    , Request (method, requestHeaders)
    , Response (responseBody, responseStatus)
    , httpLbs
    , newManager
    , parseRequest
    , urlEncodedBody
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

newtype ClientId = ClientId Text
    deriving stock (Eq, Show)

newtype Scope = Scope Text
    deriving stock (Eq, Show)

newtype OAuthToken = OAuthToken {unOAuthToken :: ByteString}
    deriving stock (Eq, Show)

data DeviceCodeResponse = DeviceCodeResponse
    { dcVerificationUri :: Text
    , dcUserCode :: Text
    , dcExpiresIn :: Int
    , dcInterval :: Int
    }
    deriving stock (Eq, Show)

data DeviceFlowError
    = ExpiredToken
    | AccessDenied
    | UnexpectedStatus Int Text
    | NetworkError Text
    deriving stock (Eq, Show)

data DeviceFlowEndpoints = DeviceFlowEndpoints
    { deviceCodeEndpoint :: String
    , accessTokenEndpoint :: String
    }
    deriving stock (Eq, Show)

githubDeviceFlowEndpoints :: DeviceFlowEndpoints
githubDeviceFlowEndpoints =
    DeviceFlowEndpoints
        { deviceCodeEndpoint = "https://github.com/login/device/code"
        , accessTokenEndpoint = "https://github.com/login/oauth/access_token"
        }

runDeviceFlowWith
    :: MonadIO m
    => DeviceFlowEndpoints
    -> (Int -> m ())
    -> ClientId
    -> [Scope]
    -> (DeviceCodeResponse -> m ())
    -> m (Either DeviceFlowError OAuthToken)
runDeviceFlowWith endpoints delay clientId scopes callback = do
    manager <- liftIO $ newManager tlsManagerSettings
    deviceCodeResult <-
        liftIO
            $ requestDeviceCode
                manager
                (deviceCodeEndpoint endpoints)
                clientId
                scopes
    case deviceCodeResult of
        Left err -> pure $ Left err
        Right deviceCode -> do
            let response = toDeviceCodeResponse deviceCode
            callback response
            pollForToken
                manager
                (accessTokenEndpoint endpoints)
                delay
                clientId
                (deviceCodeValue deviceCode)
                (dcInterval response)

requestDeviceCode
    :: Manager
    -> String
    -> ClientId
    -> [Scope]
    -> IO (Either DeviceFlowError DeviceCodeWire)
requestDeviceCode manager endpoint clientId scopes =
    postJson
        manager
        endpoint
        [ ("client_id", encodeText $ unClientId clientId)
        , ("scope", encodeText $ renderScopes scopes)
        ]

pollForToken
    :: MonadIO m
    => Manager
    -> String
    -> (Int -> m ())
    -> ClientId
    -> Text
    -> Int
    -> m (Either DeviceFlowError OAuthToken)
pollForToken manager endpoint delay clientId deviceCode interval = do
    pollResult <-
        liftIO
            $ postJson
                manager
                endpoint
                [ ("client_id", encodeText $ unClientId clientId)
                , ("device_code", encodeText deviceCode)
                ,
                    ( "grant_type"
                    , "urn:ietf:params:oauth:grant-type:device_code"
                    )
                ]
    case pollResult of
        Left err -> pure $ Left err
        Right (PollAccessToken token) ->
            pure $ Right $ OAuthToken $ encodeText token
        Right (PollError "authorization_pending") -> do
            delay interval
            pollForToken manager endpoint delay clientId deviceCode interval
        Right (PollError "slow_down") -> do
            let nextInterval = interval + 5
            delay nextInterval
            pollForToken manager endpoint delay clientId deviceCode nextInterval
        Right (PollError "expired_token") ->
            pure $ Left ExpiredToken
        Right (PollError "access_denied") ->
            pure $ Left AccessDenied
        Right (PollError err) ->
            pure $ Left $ NetworkError $ "GitHub device-flow error: " <> err

postJson
    :: FromJSON a
    => Manager
    -> String
    -> [(ByteString, ByteString)]
    -> IO (Either DeviceFlowError a)
postJson manager endpoint form = do
    requestResult <- try @HttpException $ do
        baseRequest <- parseRequest endpoint
        let request =
                urlEncodedBody form
                    baseRequest
                        { method = "POST"
                        , requestHeaders =
                            ("Accept", "application/json")
                                : requestHeaders baseRequest
                        }
        httpLbs request manager
    pure $ case requestResult of
        Left err -> Left $ NetworkError $ T.pack $ show err
        Right response ->
            let body = responseBody response
                code = statusCode $ responseStatus response
             in if code == 200
                    then decodeBody body
                    else Left $ UnexpectedStatus code $ decodeBodyText body

decodeBody :: FromJSON a => LBS.ByteString -> Either DeviceFlowError a
decodeBody body =
    case eitherDecode body of
        Left err -> Left $ NetworkError $ T.pack err
        Right value -> Right value

decodeBodyText :: LBS.ByteString -> Text
decodeBodyText = T.decodeUtf8With lenientDecode . LBS.toStrict
  where
    lenientDecode _ _ = Just '\xfffd'

encodeText :: Text -> ByteString
encodeText = T.encodeUtf8

renderScopes :: [Scope] -> Text
renderScopes scopes =
    T.intercalate " " [scope | Scope scope <- scopes]

unClientId :: ClientId -> Text
unClientId (ClientId clientId) = clientId

data DeviceCodeWire = DeviceCodeWire
    { deviceCodeValue :: Text
    , userCodeValue :: Text
    , verificationUriValue :: Text
    , expiresInValue :: Int
    , intervalValue :: Int
    }
    deriving stock (Eq, Show)

instance FromJSON DeviceCodeWire where
    parseJSON =
        withObject "DeviceCodeResponse" $ \objectValue ->
            DeviceCodeWire
                <$> objectValue .: "device_code"
                <*> objectValue .: "user_code"
                <*> objectValue .: "verification_uri"
                <*> objectValue .: "expires_in"
                <*> objectValue .: "interval"

toDeviceCodeResponse :: DeviceCodeWire -> DeviceCodeResponse
toDeviceCodeResponse DeviceCodeWire{..} =
    DeviceCodeResponse
        { dcVerificationUri = verificationUriValue
        , dcUserCode = userCodeValue
        , dcExpiresIn = expiresInValue
        , dcInterval = intervalValue
        }

data PollResponse
    = PollAccessToken Text
    | PollError Text
    deriving stock (Eq, Show)

instance FromJSON PollResponse where
    parseJSON =
        withObject "DeviceFlowPollResponse" $ \objectValue ->
            (PollAccessToken <$> objectValue .: "access_token")
                <|> (PollError <$> objectValue .: "error")
