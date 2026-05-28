-- | Internal client for the GitHub identity API.
--
-- This module exposes the response-mapping logic and an injectable
-- endpoint so tests can point the client at a local fixture server
-- while the public "Lib.GitHub.Auth.Identify" wrapper targets GitHub.
--
-- The 'Login' newtype lives here (rather than in the public module) so
-- that 'whoamiWith' can return it without an import cycle; the public
-- "Lib.GitHub.Auth.Identify" re-exports it unchanged.
module Lib.GitHub.Auth.Identify.Internal
    ( -- * Public types
      Login (..)
    , GitHubError (..)

      -- * Injectable client
    , IdentifyEndpoint (..)
    , githubIdentifyEndpoint
    , whoamiWith
    ) where

import Control.Exception (try)
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON
    , eitherDecode
    , withObject
    , (.:)
    )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Network.HTTP.Client
    ( HttpException
    , Request (method, requestHeaders)
    , Response (responseBody, responseStatus)
    , httpLbs
    , newManager
    , parseRequest
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAccept, hAuthorization, hUserAgent)
import Network.HTTP.Types.Status (statusCode)

-- | A GitHub login (username). A distinct newtype so it cannot be
-- confused with an 'Lib.GitHub.Auth.TeamCheck.Org' or
-- 'Lib.GitHub.Auth.TeamCheck.TeamSlug' at a call site.
newtype Login = Login {unLogin :: Text}
    deriving stock (Show, Eq, Ord)
    deriving newtype (IsString, ToJSON, FromJSON)

-- | A failure at the GitHub identity boundary, carrying the HTTP status
-- code and the raw response body for diagnosis. A status of @0@ denotes
-- a transport-level failure.
data GitHubError = GitHubError
    { status :: Int
    , body :: Text
    }
    deriving stock (Eq, Show)

-- | Base URL the identity client targets. Injected so tests can point
-- at a local fixture server.
newtype IdentifyEndpoint = IdentifyEndpoint
    { identifyBaseUrl :: String
    }
    deriving stock (Eq, Show)

-- | The production GitHub REST API base URL.
githubIdentifyEndpoint :: IdentifyEndpoint
githubIdentifyEndpoint =
    IdentifyEndpoint{identifyBaseUrl = "https://api.github.com"}

-- | Look up the authenticated user against an injectable endpoint.
-- The OAuth token is sent as an @Authorization: Bearer@ header; its
-- bytes are never logged.
whoamiWith
    :: IdentifyEndpoint
    -> OAuthToken
    -> IO (Either GitHubError Login)
whoamiWith endpoint token = do
    manager <- newManager tlsManagerSettings
    requestResult <- try @HttpException $ do
        baseRequest <- parseRequest $ identifyBaseUrl endpoint <> "/user"
        let request =
                baseRequest
                    { method = "GET"
                    , requestHeaders =
                        [ (hAccept, "application/vnd.github+json")
                        , (hAuthorization, "Bearer " <> unOAuthToken token)
                        , (hUserAgent, "moog")
                        ]
                    }
        httpLbs request manager
    pure $ case requestResult of
        Left err -> Left $ GitHubError 0 $ T.pack $ show err
        Right response -> classifyResponse response

-- | Map a raw HTTP response to either a 'Login' or a 'GitHubError'.
classifyResponse :: Response LBS.ByteString -> Either GitHubError Login
classifyResponse response =
    case statusCode (responseStatus response) of
        200 -> case decodeLogin rawBody of
            Right login -> Right login
            Left _ -> Left $ GitHubError 200 bodyText
        code -> Left $ GitHubError code bodyText
  where
    rawBody = responseBody response
    bodyText = decodeBodyText rawBody

-- | The @login@ field of a @GET /user@ payload.
newtype UserLogin = UserLogin Login

instance FromJSON UserLogin where
    parseJSON =
        withObject "UserLogin" $ \o ->
            UserLogin <$> o .: "login"

-- | Decode the @login@ field of a 200 @GET /user@ payload.
decodeLogin :: LBS.ByteString -> Either String Login
decodeLogin raw =
    (\(UserLogin login) -> login) <$> eitherDecode raw

decodeBodyText :: LBS.ByteString -> Text
decodeBodyText = decodeUtf8Lenient . LBS.toStrict

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = T.decodeUtf8With lenientDecode
  where
    lenientDecode _ _ = Just '\xfffd'
