{-# LANGUAGE OverloadedStrings #-}

-- | Bearer-token authorization middleware for the Antithesis proxy.
module Proxy.Antithesis.Middleware.Auth
    ( AuthConfig (..)
    , authMiddleware
    , authorizedLoginKey
    )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Time.Clock (UTCTime)
import Data.Vault.Lazy qualified as Vault
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Lib.GitHub.Auth.Identify
    ( GitHubError (..)
    , Login
    )
import Lib.GitHub.Auth.TeamCheck
    ( MembershipResult (..)
    , Org
    , TeamSlug
    )
import Network.HTTP.Types
    ( ResponseHeaders
    , Status
    , hAuthorization
    , status401
    , status403
    , status502
    )
import Network.Wai
    ( Middleware
    , Request (requestHeaders, vault)
    , Response
    , responseLBS
    )
import Proxy.Antithesis.Cache
    ( MembershipCache
    , cachedMembership
    )
import System.IO.Unsafe (unsafePerformIO)

data AuthConfig = AuthConfig
    { authOrg :: Org
    , authTeam :: TeamSlug
    , authCache :: MembershipCache
    , authNow :: IO UTCTime
    , authWhoami :: OAuthToken -> IO (Either GitHubError Login)
    , authCheckTeamMembership
        :: OAuthToken -> Org -> TeamSlug -> Login -> IO MembershipResult
    }

authorizedLoginKey :: Vault.Key Login
authorizedLoginKey = unsafePerformIO Vault.newKey
{-# NOINLINE authorizedLoginKey #-}

authMiddleware :: AuthConfig -> Middleware
authMiddleware config app request respond =
    case bearerToken request of
        Nothing ->
            respond unauthorizedBearer
        Just token -> do
            authWhoami config token >>= \case
                Left GitHubError{status = 401} ->
                    respond $ plain status401 [] "invalid token"
                Left _ ->
                    respond $ plain status502 [] "github identity check failed"
                Right login -> do
                    membership <-
                        cachedMembership
                            (authCache config)
                            (authNow config)
                            token
                            ( authCheckTeamMembership
                                config
                                token
                                (authOrg config)
                                (authTeam config)
                                login
                            )
                    case membership of
                        Active ->
                            app
                                request
                                    { vault =
                                        Vault.insert
                                            authorizedLoginKey
                                            login
                                            (vault request)
                                    }
                                respond
                        Pending ->
                            respond $ plain status403 [] "forbidden"
                        NotMember ->
                            respond $ plain status403 [] "forbidden"
                        TokenInvalid ->
                            respond $ plain status401 [] "invalid token"
                        SSORequired{url} ->
                            respond
                                $ plain
                                    status403
                                    [("X-Moog-SSO-Url", textUtf8 url)]
                                    "sso required"
                        OtherError{} ->
                            respond
                                $ plain
                                    status502
                                    []
                                    "github membership check failed"

bearerToken :: Request -> Maybe OAuthToken
bearerToken request =
    case lookup hAuthorization $ requestHeaders request of
        Just raw
            | ["Bearer", token] <- BC.words raw
            , not (BC.null token) ->
                Just $ OAuthToken token
        _ -> Nothing

unauthorizedBearer :: Response
unauthorizedBearer =
    plain
        status401
        [("WWW-Authenticate", "Bearer realm=\"moog-antithesis-proxy\"")]
        "missing bearer token"

plain
    :: Status
    -> ResponseHeaders
    -> LBS.ByteString
    -> Response
plain status headers body =
    responseLBS status headers body

textUtf8 :: Text -> ByteString
textUtf8 = T.encodeUtf8
