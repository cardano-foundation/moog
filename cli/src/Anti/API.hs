{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Anti.API (api, API, request) where

import Anti.Types (Request, TokenId)
import Data.Aeson (Value)
import Data.Data (Proxy (..))
import Servant.API (Capture, JSON, Post, ReqBody, type (:>))
import Servant.Client (ClientM, client)

type API =
    "token"
        :> Capture "tokenId" TokenId
        :> "request"
        :> ReqBody '[JSON] Request
        :> Post '[JSON] Value

api :: Proxy API
api = Proxy

request
    :: TokenId
    -> Request
    -> ClientM Value
request = client api