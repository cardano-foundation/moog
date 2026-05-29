{-# LANGUAGE OverloadedStrings #-}

module Proxy.Antithesis.AuthSpec
    ( spec
    )
where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Vault.Lazy qualified as Vault
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Lib.GitHub.Auth.Identify (GitHubError (..), Login (..))
import Lib.GitHub.Auth.TeamCheck
    ( MembershipResult (..)
    , Org (..)
    , TeamSlug (..)
    )
import Network.HTTP.Types
    ( hAuthorization
    , HeaderName
    , status200
    , status401
    , status403
    , status502
    )
import Network.Wai
    ( Application
    , Middleware
    , Request (requestHeaders)
    , responseLBS
    , vault
    )
import Network.Wai.Test
    ( SResponse (..)
    , defaultRequest
    , request
    , runSession
    )
import Proxy.Antithesis.Cache (newMembershipCache)
import Proxy.Antithesis.Middleware.Auth
    ( AuthConfig (..)
    , authMiddleware
    , authorizedLoginKey
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

spec :: Spec
spec =
    describe "Proxy.Antithesis.Middleware.Auth" $ do
        it "rejects a missing bearer token with a challenge" $ do
            response <- exercise Active []
            simpleStatus response `shouldBe` status401
            lookup "WWW-Authenticate" (simpleHeaders response)
                `shouldBe` Just "Bearer realm=\"moog-antithesis-proxy\""

        it "rejects a malformed bearer token with a challenge" $ do
            response <- exercise Active [(hAuthorization, "Basic abc")]
            simpleStatus response `shouldBe` status401
            lookup "WWW-Authenticate" (simpleHeaders response)
                `shouldBe` Just "Bearer realm=\"moog-antithesis-proxy\""

        it "maps whoami 401 to 401" $ do
            response <-
                exerciseWith
                    (Left $ GitHubError 401 "bad credentials")
                    Active
                    bearer
            simpleStatus response `shouldBe` status401

        it "maps membership TokenInvalid to 401" $ do
            response <- exercise TokenInvalid bearer
            simpleStatus response `shouldBe` status401

        it "maps Pending to 403" $ do
            response <- exercise Pending bearer
            simpleStatus response `shouldBe` status403

        it "maps NotMember to 403" $ do
            response <- exercise NotMember bearer
            simpleStatus response `shouldBe` status403

        it "maps SSORequired to 403 with the SSO URL header" $ do
            response <- exercise (SSORequired ssoUrl) bearer
            simpleStatus response `shouldBe` status403
            lookup "X-Moog-SSO-Url" (simpleHeaders response)
                `shouldBe` Just (T.encodeUtf8 ssoUrl)

        it "maps OtherError to a sanitized 502" $ do
            response <- exercise (OtherError 500 "upstream secret") bearer
            simpleStatus response `shouldBe` status502
            simpleBody response `shouldBe` "github membership check failed"

        it "attaches the authorized login to the request vault" $ do
            response <- exercise Active bearer
            simpleStatus response `shouldBe` status200
            simpleBody response `shouldBe` "octocat"

        it "reuses a cached membership result for the same token prefix" $ do
            callsRef <- newIORef (0 :: Int)
            middleware <- mkMiddleware RightOctocat Active callsRef
            first <- runAuth middleware bearer
            second <- runAuth middleware bearer
            calls <- readIORef callsRef

            simpleStatus first `shouldBe` status200
            simpleStatus second `shouldBe` status200
            calls `shouldBe` 1

bearer :: [(HeaderName, ByteString)]
bearer = [(hAuthorization, "Bearer test-token")]

ssoUrl :: Text
ssoUrl = "https://github.com/orgs/pragma-org/sso"

data WhoamiResult = RightOctocat | WhoamiFailure GitHubError

exercise :: MembershipResult -> [(HeaderName, ByteString)] -> IO SResponse
exercise = exerciseWith (Right $ Login "octocat")

exerciseWith
    :: Either GitHubError Login
    -> MembershipResult
    -> [(HeaderName, ByteString)]
    -> IO SResponse
exerciseWith whoamiResult membership headers = do
    callsRef <- newIORef (0 :: Int)
    middleware <-
        mkMiddleware
            (either WhoamiFailure (const RightOctocat) whoamiResult)
            membership
            callsRef
    runAuthWithWhoami middleware whoamiResult headers

mkMiddleware
    :: WhoamiResult
    -> MembershipResult
    -> IORef Int
    -> IO Middleware
mkMiddleware whoamiResult membership callsRef = do
    cache <- newMembershipCache 60
    pure $
        authMiddleware
            AuthConfig
                { authOrg = Org "pragma-org"
                , authTeam = TeamSlug "antithesis-access"
                , authCache = cache
                , authNow = pure baseTime
                , authWhoami = const $ pure $ case whoamiResult of
                    RightOctocat -> Right $ Login "octocat"
                    WhoamiFailure err -> Left err
                , authCheckTeamMembership = \_ _ _ _ -> do
                    calls <- readIORef callsRef
                    writeIORef callsRef $ calls + 1
                    pure membership
                }

runAuth :: Middleware -> [(HeaderName, ByteString)] -> IO SResponse
runAuth middleware = runAuthWithWhoami middleware (Right $ Login "octocat")

runAuthWithWhoami
    :: Middleware
    -> Either GitHubError Login
    -> [(HeaderName, ByteString)]
    -> IO SResponse
runAuthWithWhoami middleware _whoamiResult headers =
    runSession
        ( request
            defaultRequest{requestHeaders = headers}
        )
        (middleware protectedApp)

protectedApp :: Application
protectedApp request' respond =
    case Vault.lookup authorizedLoginKey (vault request') of
        Just (Login login) ->
            respond
                $ responseLBS
                    status200
                    []
                    (LBS.fromStrict $ T.encodeUtf8 login)
        Nothing ->
            respond $ responseLBS status502 [] "missing login"

baseTime :: UTCTime
baseTime = UTCTime (toEnum 0) (secondsToDiffTime 0)
