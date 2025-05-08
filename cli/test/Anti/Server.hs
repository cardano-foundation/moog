{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Anti.Server
    ( appDummy
    , dummyTxId
    )
    where

import Anti.API (api)
import Anti.Types (Request (..), TokenId)
import Data.Aeson (Value, object, (.=))
import Network.Wai (Application)
import Servant (serve)
import Servant.Server (Handler)

appDummy :: Application
appDummy = serve api serverDummy

dummyTxId :: Value
dummyTxId = object ["txId " .= ("dummyTxId" :: String)]

serverDummy :: TokenId -> Request -> Handler Value
serverDummy _tokenId = \case
    Request
        { key = _
        , value = _
        , operation = _
        } -> do return dummyTxId
    -- _ -> error "Invalid request format"
