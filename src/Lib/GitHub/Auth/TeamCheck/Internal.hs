-- | Internal client for the GitHub team-membership API.
--
-- This module exposes the response-mapping logic and an injectable
-- endpoint so tests can point the client at a local fixture server
-- while the public "Lib.GitHub.Auth.TeamCheck" wrapper targets GitHub.
module Lib.GitHub.Auth.TeamCheck.Internal
    ( -- * Public types
      Org (..)
    , TeamSlug (..)
    , MembershipResult (..)

      -- * Injectable client
    , TeamCheckEndpoint (..)
    , githubTeamCheckEndpoint
    , checkTeamMembershipWith

      -- * Helpers
    , parseSsoUrl
    ) where

import Control.Exception (try)
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
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Network.HTTP.Client
    ( HttpException
    , Request (method, requestHeaders)
    , Response (responseBody, responseHeaders, responseStatus)
    , httpLbs
    , newManager
    , parseRequest
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAccept, hAuthorization, hUserAgent)
import Network.HTTP.Types.Status (statusCode)

-- | A GitHub organisation login.
newtype Org = Org Text
    deriving stock (Eq, Show)

-- | A GitHub team slug within an organisation.
newtype TeamSlug = TeamSlug Text
    deriving stock (Eq, Show)

-- | Outcome of a team-membership check.
data MembershipResult
    = -- | The login is an active member of the team.
      Active
    | -- | The login has a pending invitation to the team.
      Pending
    | -- | The login is not a member of the team.
      NotMember
    | -- | The supplied token was rejected.
      TokenInvalid
    | -- | SAML SSO authorisation is required; carries the URL to visit.
      SSORequired {url :: Text}
    | -- | Any other response, carrying the status code and raw body.
      OtherError {status :: Int, body :: Text}
    deriving stock (Eq, Show)

-- | Base URL the membership client targets. Injected so tests can
-- point at a local fixture server.
newtype TeamCheckEndpoint = TeamCheckEndpoint
    { teamCheckBaseUrl :: String
    }
    deriving stock (Eq, Show)

-- | The production GitHub REST API base URL.
githubTeamCheckEndpoint :: TeamCheckEndpoint
githubTeamCheckEndpoint =
    TeamCheckEndpoint{teamCheckBaseUrl = "https://api.github.com"}

-- | Check team membership against an injectable endpoint.
checkTeamMembershipWith
    :: TeamCheckEndpoint
    -> OAuthToken
    -> Org
    -> TeamSlug
    -> Text
    -> IO MembershipResult
checkTeamMembershipWith endpoint token (Org org) (TeamSlug team) login = do
    manager <- newManager tlsManagerSettings
    requestResult <- try @HttpException $ do
        baseRequest <-
            parseRequest $ membershipUrl endpoint org team login
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
        Left err -> OtherError 0 $ T.pack $ show err
        Right response -> classifyResponse response

-- | Build the membership URL for a login within a team.
membershipUrl :: TeamCheckEndpoint -> Text -> Text -> Text -> String
membershipUrl endpoint org team login =
    teamCheckBaseUrl endpoint
        <> "/orgs/"
        <> T.unpack org
        <> "/teams/"
        <> T.unpack team
        <> "/memberships/"
        <> T.unpack login

-- | Map a raw HTTP response to a 'MembershipResult'.
classifyResponse :: Response LBS.ByteString -> MembershipResult
classifyResponse response =
    case statusCode (responseStatus response) of
        200 -> case decodeState rawBody of
            Right "active" -> Active
            Right "pending" -> Pending
            _ -> OtherError 200 bodyText
        401 -> TokenInvalid
        403 -> case ssoUrl of
            Just ssoTarget -> SSORequired ssoTarget
            Nothing -> OtherError 403 bodyText
        404 -> NotMember
        code -> OtherError code bodyText
  where
    rawBody = responseBody response
    bodyText = decodeBodyText rawBody
    ssoUrl = do
        rawHeader <- lookup "X-GitHub-SSO" (responseHeaders response)
        parseSsoUrl $ decodeUtf8Lenient rawHeader

-- | The @state@ field of a membership payload.
newtype MembershipState = MembershipState Text

instance FromJSON MembershipState where
    parseJSON =
        withObject "MembershipState" $ \o ->
            MembershipState <$> o .: "state"

-- | Decode the @state@ field of a 200 membership payload.
decodeState :: LBS.ByteString -> Either String Text
decodeState raw =
    (\(MembershipState state) -> state) <$> eitherDecode raw

-- | Extract the SSO target URL from an @X-GitHub-SSO@ header value,
-- preserving the URL verbatim and dropping only the @url=@ prefix and
-- any leading metadata such as @required; @.
parseSsoUrl :: Text -> Maybe Text
parseSsoUrl headerValue =
    case T.breakOn "url=" headerValue of
        (_, rest)
            | not (T.null rest) -> Just $ T.drop (T.length "url=") rest
        _ -> Nothing

decodeBodyText :: LBS.ByteString -> Text
decodeBodyText = decodeUtf8Lenient . LBS.toStrict

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = T.decodeUtf8With lenientDecode
  where
    lenientDecode _ _ = Just '\xfffd'
