{-# LANGUAGE OverloadedStrings #-}

module Lib.GitHub.Auth.TeamCheckSpec
    ( spec
    )
where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Lib.GitHub.Auth.TeamCheck
    ( MembershipResult (..)
    , Org (..)
    , TeamSlug (..)
    )
import Lib.GitHub.Auth.TeamCheck.Internal
    ( TeamCheckEndpoint (..)
    , checkTeamMembershipWith
    )
import Network.HTTP.Types
    ( ResponseHeaders
    , Status
    , hContentType
    , status200
    , status401
    , status403
    , status404
    , status500
    )
import Network.Wai
    ( Application
    , responseLBS
    )
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

spec :: Spec
spec = do
    describe "checkTeamMembershipWith" $ do
        it "maps 200 active to Active" $ do
            result <-
                withMockTeam
                    status200
                    [(hContentType, "application/json")]
                    (jsonResponse $ object ["state" .= ("active" :: Text)])
                    runCheck
            result `shouldBe` Active

        it "maps 200 pending to Pending" $ do
            result <-
                withMockTeam
                    status200
                    [(hContentType, "application/json")]
                    (jsonResponse $ object ["state" .= ("pending" :: Text)])
                    runCheck
            result `shouldBe` Pending

        it "maps 404 to NotMember" $ do
            result <-
                withMockTeam status404 [] "" runCheck
            result `shouldBe` NotMember

        it "maps 401 to TokenInvalid" $ do
            result <-
                withMockTeam status401 [] "" runCheck
            result `shouldBe` TokenInvalid

        it "maps 403 with X-GitHub-SSO to SSORequired carrying the verbatim url" $ do
            result <-
                withMockTeam
                    status403
                    [ ( "X-GitHub-SSO"
                      , "required; url=https://github.com/orgs/myorg/sso?authorization_request=ABC123"
                      )
                    ]
                    ""
                    runCheck
            result
                `shouldBe` SSORequired
                    "https://github.com/orgs/myorg/sso?authorization_request=ABC123"

        it "maps 500 to OtherError carrying status and body" $ do
            result <-
                withMockTeam status500 [] "boom" runCheck
            result `shouldBe` OtherError 500 "boom"

runCheck :: TeamCheckEndpoint -> IO MembershipResult
runCheck endpoint =
    checkTeamMembershipWith
        endpoint
        (OAuthToken "test-token")
        (Org "myorg")
        (TeamSlug "myteam")
        "octocat"

withMockTeam
    :: Status
    -> ResponseHeaders
    -> LBS.ByteString
    -> (TeamCheckEndpoint -> IO a)
    -> IO a
withMockTeam status headers body action =
    testWithApplication (pure $ application status headers body) $ \port ->
        action
            TeamCheckEndpoint
                { teamCheckBaseUrl = "http://127.0.0.1:" <> show port
                }

application :: Status -> ResponseHeaders -> LBS.ByteString -> Application
application status headers body _request respond =
    respond $ responseLBS status headers body

jsonResponse :: Value -> LBS.ByteString
jsonResponse = encode
