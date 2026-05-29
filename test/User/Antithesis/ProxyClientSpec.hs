{-# LANGUAGE OverloadedStrings #-}

module User.Antithesis.ProxyClientSpec
    ( spec
    )
where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Network.HTTP.Types
    ( hAuthorization
    , status200
    , status401
    )
import Network.Wai
    ( Application
    , Request (requestHeaders)
    , responseLBS
    )
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import User.Antithesis.ProxyClient
    ( ProxyUrl (..)
    , runsRequestWithRefresh
    )

spec :: Spec
spec =
    describe "User.Antithesis.ProxyClient" $
        it "evicts the cached token and retries once after 401" $ do
            seenAuthRef <- newIORef []
            evictionsRef <- newIORef (0 :: Int)
            tokensRef <- newIORef [OAuthToken "stale-token", OAuthToken "fresh-token"]

            result <-
                withRunsProxy seenAuthRef $ \baseUrl ->
                    runsRequestWithRefresh
                        (ProxyUrl baseUrl)
                        (popToken tokensRef)
                        (increment evictionsRef)

            result `shouldBe` Right (Aeson.object ["runs" Aeson..= ([] :: [Aeson.Value])])
            seenAuth <- readIORef seenAuthRef
            seenAuth `shouldBe` ["Bearer stale-token", "Bearer fresh-token"]
            evictions <- readIORef evictionsRef
            evictions `shouldBe` 1

withRunsProxy :: IORef [ByteString] -> (String -> IO a) -> IO a
withRunsProxy seenAuthRef action =
    testWithApplication (pure $ runsProxy seenAuthRef) $ \port ->
        action $ "http://127.0.0.1:" <> show port

runsProxy :: IORef [ByteString] -> Application
runsProxy seenAuthRef request respond = do
    let auth = lookup hAuthorization $ requestHeaders request
    writeIORef seenAuthRef . (<> maybe [] (: []) auth) =<< readIORef seenAuthRef
    case auth of
        Just "Bearer fresh-token" ->
            respond
                $ responseLBS
                    status200
                    [("Content-Type", "application/json")]
                    "{\"runs\":[]}"
        _ ->
            respond $ responseLBS status401 [] "bad credentials"

popToken :: IORef [OAuthToken] -> IO OAuthToken
popToken tokensRef =
    atomicModifyIORef' tokensRef $ \case
        [] -> ([], error "no token left")
        token : rest -> (rest, token)

increment :: IORef Int -> IO ()
increment ref =
    atomicModifyIORef' ref $ \count -> (count + 1, ())
