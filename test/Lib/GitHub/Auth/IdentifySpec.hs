{-# LANGUAGE OverloadedStrings #-}

module Lib.GitHub.Auth.IdentifySpec
    ( spec
    )
where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Lib.GitHub.Auth.Identify (GitHubError (..), Login (..))
import Lib.GitHub.Auth.Identify.Internal
    ( IdentifyEndpoint (..)
    , whoamiWith
    )
import Network.HTTP.Types
    ( ResponseHeaders
    , Status
    , hContentType
    , status200
    , status401
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
    describe "whoamiWith" $ do
        it "maps 200 {login} to Right (Login ...)" $ do
            result <-
                withMockUser
                    status200
                    [(hContentType, "application/json")]
                    (jsonResponse $ object ["login" .= ("octocat" :: Text)])
                    runWhoami
            result `shouldBe` Right (Login "octocat")

        it "maps a non-success response to Left GitHubError" $ do
            result <-
                withMockUser status401 [] "bad creds" runWhoami
            result `shouldBe` Left (GitHubError 401 "bad creds")

runWhoami :: IdentifyEndpoint -> IO (Either GitHubError Login)
runWhoami endpoint =
    whoamiWith endpoint (OAuthToken "test-token")

withMockUser
    :: Status
    -> ResponseHeaders
    -> LBS.ByteString
    -> (IdentifyEndpoint -> IO a)
    -> IO a
withMockUser status headers body action =
    testWithApplication (pure $ application status headers body) $ \port ->
        action
            IdentifyEndpoint
                { identifyBaseUrl = "http://127.0.0.1:" <> show port
                }

application :: Status -> ResponseHeaders -> LBS.ByteString -> Application
application status headers body _request respond =
    respond $ responseLBS status headers body

jsonResponse :: Value -> LBS.ByteString
jsonResponse = encode
