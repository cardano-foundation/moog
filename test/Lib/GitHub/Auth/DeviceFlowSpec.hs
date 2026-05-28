{-# LANGUAGE OverloadedStrings #-}

module Lib.GitHub.Auth.DeviceFlowSpec
    ( spec
    )
where

import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Monad (forM_)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Lib.GitHub.Auth.DeviceFlow
    ( ClientId (..)
    , DeviceCodeResponse (..)
    , DeviceFlowError (..)
    , OAuthToken (..)
    , Scope (..)
    )
import Lib.GitHub.Auth.DeviceFlow.Internal
    ( DeviceFlowEndpoints (..)
    , runDeviceFlowWith
    )
import Network.HTTP.Types
    ( Query
    , Status
    , hContentType
    , methodPost
    , parseQuery
    , status200
    , status404
    )
import Network.Wai
    ( Application
    , Request (rawPathInfo, requestMethod)
    , Response
    , responseLBS
    , strictRequestBody
    )
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

data RecordedRequest = RecordedRequest
    { recordedPath :: ByteString
    , recordedBody :: LBS.ByteString
    }
    deriving stock (Eq, Show)

spec :: Spec
spec = do
    describe "runDeviceFlowWith" $ do
        it "keeps polling after authorization_pending and returns the access token" $ do
            outcome <-
                withMockGitHub
                    [ jsonResponse $ object ["error" .= ("authorization_pending" :: Text)]
                    , jsonResponse $ object ["access_token" .= ("token-one" :: Text)]
                    ]
                    $ \endpoints requests -> do
                        delays <- newMVar []
                        result <-
                            runDeviceFlowWith
                                endpoints
                                (recordDelay delays)
                                (ClientId "client-one")
                                [Scope "repo"]
                                (const $ pure ())
                        (,,)
                            <$> pure result
                            <*> readMVar delays
                            <*> readMVar requests

            let (result, delays, requests) = outcome
            result `shouldBe` Right (OAuthToken "token-one")
            delays `shouldBe` [1]
            requestPaths requests
                `shouldBe` ["/login/device/code", "/login/oauth/access_token", "/login/oauth/access_token"]

        it "increases the next polling delay by five seconds after slow_down" $ do
            outcome <-
                withMockGitHub
                    [ jsonResponse $ object ["error" .= ("slow_down" :: Text)]
                    , jsonResponse $ object ["access_token" .= ("token-two" :: Text)]
                    ]
                    $ \endpoints _requests -> do
                        delays <- newMVar []
                        result <-
                            runDeviceFlowWith
                                endpoints
                                (recordDelay delays)
                                (ClientId "client-two")
                                [Scope "repo"]
                                (const $ pure ())
                        (result,) <$> readMVar delays

            outcome `shouldBe` (Right (OAuthToken "token-two"), [6])

        it "returns ExpiredToken for expired_token polling responses" $ do
            result <-
                withMockGitHub
                    [jsonResponse $ object ["error" .= ("expired_token" :: Text)]]
                    $ \endpoints _requests ->
                        runDeviceFlowWith
                            endpoints
                            (const $ pure ())
                            (ClientId "client-three")
                            [Scope "repo"]
                            (const $ pure ())

            result `shouldBe` Left ExpiredToken

        it "returns AccessDenied for access_denied polling responses" $ do
            result <-
                withMockGitHub
                    [jsonResponse $ object ["error" .= ("access_denied" :: Text)]]
                    $ \endpoints _requests ->
                        runDeviceFlowWith
                            endpoints
                            (const $ pure ())
                            (ClientId "client-four")
                            [Scope "repo"]
                            (const $ pure ())

            result `shouldBe` Left AccessDenied

        it "maps malformed JSON responses to NetworkError" $ do
            result <-
                withMockGitHub ["not-json"] $ \endpoints _requests ->
                    runDeviceFlowWith
                        endpoints
                        (const $ pure ())
                        (ClientId "client-five")
                        [Scope "repo"]
                        (const $ pure ())

            result `shouldSatisfy` \case
                Left (NetworkError message) -> not (null $ show message)
                _ -> False

        it "passes the complete device-code payload to the callback" $ do
            callbackPayload <-
                withMockGitHub
                    [jsonResponse $ object ["access_token" .= ("token-three" :: Text)]]
                    $ \endpoints _requests -> do
                        payloads <- newMVar []
                        _ <-
                            runDeviceFlowWith
                                endpoints
                                (const $ pure ())
                                (ClientId "client-six")
                                [Scope "repo"]
                                (\payload -> modifyMVar_ payloads $ \xs -> pure (xs <> [payload]))
                        readMVar payloads

            callbackPayload
                `shouldBe` [ DeviceCodeResponse
                                { dcVerificationUri = "https://github.com/login/device"
                                , dcUserCode = "ABCD-EFGH"
                                , dcExpiresIn = 900
                                , dcInterval = 1
                                }
                           ]

        it "uses the explicit ClientId parameter in every request body" $ do
            requests <-
                withMockGitHub
                    [jsonResponse $ object ["access_token" .= ("token-four" :: Text)]]
                    $ \endpoints requestLog -> do
                        _ <-
                            runDeviceFlowWith
                                endpoints
                                (const $ pure ())
                                (ClientId "explicit-client")
                                [Scope "repo", Scope "read:user"]
                                (const $ pure ())
                        readMVar requestLog

            forM_ requests $ \request ->
                formBody request `lookupParam` "client_id" `shouldBe` Just "explicit-client"
            let deviceRequest =
                    fromMaybe (error "missing device-code request")
                        $ find ((== "/login/device/code") . recordedPath) requests
            formBody deviceRequest `lookupParam` "scope" `shouldBe` Just "repo read:user"

withMockGitHub
    :: [LBS.ByteString]
    -> (DeviceFlowEndpoints -> MVar [RecordedRequest] -> IO a)
    -> IO a
withMockGitHub tokenResponses action = do
    requests <- newMVar []
    responses <- newMVar tokenResponses
    testWithApplication (pure $ application requests responses) $ \port ->
        action
            DeviceFlowEndpoints
                { deviceCodeEndpoint =
                    "http://127.0.0.1:" <> show port <> "/login/device/code"
                , accessTokenEndpoint =
                    "http://127.0.0.1:" <> show port <> "/login/oauth/access_token"
                }
            requests

application :: MVar [RecordedRequest] -> MVar [LBS.ByteString] -> Application
application requests responses request respond = do
    body <- strictRequestBody request
    modifyMVar_ requests $ \xs ->
        pure
            $ xs
                <> [ RecordedRequest
                        { recordedPath = rawPathInfo request
                        , recordedBody = body
                        }
                   ]
    if requestMethod request == methodPost
        then respondForPath body
        else respond $ responseLBS status404 [] ""
  where
    respondForPath _body = case rawPathInfo request of
        "/login/device/code" ->
            respond $ json status200 deviceCodeResponse
        "/login/oauth/access_token" -> do
            response <- popResponse responses
            respond $ responseLBS status200 [(hContentType, "application/json")] response
        _ ->
            respond $ responseLBS status404 [] ""

deviceCodeResponse :: Value
deviceCodeResponse =
    object
        [ "device_code" .= ("device-code-from-server" :: Text)
        , "user_code" .= ("ABCD-EFGH" :: Text)
        , "verification_uri" .= ("https://github.com/login/device" :: Text)
        , "expires_in" .= (900 :: Int)
        , "interval" .= (1 :: Int)
        ]

json :: Status -> Value -> Response
json status value =
    responseLBS status [(hContentType, "application/json")] (encode value)

jsonResponse :: Value -> LBS.ByteString
jsonResponse = encode

popResponse :: MVar [LBS.ByteString] -> IO LBS.ByteString
popResponse responses =
    modifyMVar responses $ \case
        [] -> pure ([], jsonResponse $ object ["error" .= ("unexpected_poll" :: Text)])
        response : rest -> pure (rest, response)

recordDelay :: MVar [Int] -> Int -> IO ()
recordDelay delays delay =
    modifyMVar_ delays $ \xs -> pure (xs <> [delay])

requestPaths :: [RecordedRequest] -> [ByteString]
requestPaths = fmap recordedPath

formBody :: RecordedRequest -> Query
formBody = parseQuery . LBS.toStrict . recordedBody

lookupParam :: Query -> ByteString -> Maybe ByteString
lookupParam query key = joinMaybe $ lookup key query

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe = \case
    Just (Just value) -> Just value
    _ -> Nothing
