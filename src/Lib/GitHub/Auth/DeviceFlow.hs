module Lib.GitHub.Auth.DeviceFlow
    ( ClientId (..)
    , Scope (..)
    , OAuthToken (..)
    , DeviceCodeResponse (..)
    , DeviceFlowError (..)
    , runDeviceFlow
    )
where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Lib.GitHub.Auth.DeviceFlow.Internal
    ( ClientId (..)
    , DeviceCodeResponse (..)
    , DeviceFlowError (..)
    , OAuthToken (..)
    , Scope (..)
    , githubDeviceFlowEndpoints
    , runDeviceFlowWith
    )

runDeviceFlow
    :: MonadIO m
    => ClientId
    -> [Scope]
    -> (DeviceCodeResponse -> m ())
    -> m (Either DeviceFlowError OAuthToken)
runDeviceFlow =
    runDeviceFlowWith githubDeviceFlowEndpoints $ \seconds ->
        liftIO $ threadDelay $ seconds * 1000000
