{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Anti.API
    ( api
    , API
    , requestChange
    , retractRequest
    , createToken
    , deleteToken
    , getToken
    , updateToken
    ) where

import Anti.Types (Request, RequestRefs, TokenId)
import Data.Aeson (Value)
import Data.Data (Proxy (..))
import Servant.API
    ( Capture
    , Delete
    , Get
    , JSON
    , Post
    , Put
    , ReqBody
    , (:<|>) (..)
    , type (:>)
    )
import Servant.Client (ClientM, client)

type API =
    "token"
        :> Capture "tokenId" TokenId
        :> "request"
        :> ReqBody '[JSON] Request
        :> Post '[JSON] Value
        :<|> "request"
            :> Capture "txHash" String
            :> Capture "outputIndex" Int
            :> Delete '[JSON] Value
        :<|> "token"
            :> Post '[JSON] Value
        :<|> "token"
            :> Capture "tokenId" TokenId
            :> Delete '[JSON] Value
        :<|> "token"
            :> Capture "tokenId" TokenId
            :> Get '[JSON] Value
        :<|> "token"
            :> Capture "tokenId" TokenId
            :> ReqBody '[JSON] RequestRefs
            :> Put '[JSON] Value

api :: Proxy API
api = Proxy

requestChange
    :: TokenId
    -> Request
    -> ClientM Value
createToken :: ClientM Value
deleteToken :: TokenId -> ClientM Value
updateToken :: TokenId -> RequestRefs -> ClientM Value
getToken :: TokenId -> ClientM Value
retractRequest :: [Char] -> Int -> ClientM Value
requestChange
    :<|> retractRequest
    :<|> createToken
    :<|> deleteToken
    :<|> getToken
    :<|> updateToken =
        client api
