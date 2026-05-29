{-# LANGUAGE OverloadedStrings #-}

-- | Handler for proxying Antithesis run listings.
--
-- Forwards client `GET /api/v0/runs` to the same path on the Antithesis
-- upstream, attaching the server-held Antithesis API key as a
-- `Authorization: Bearer …` header.
module Proxy.Antithesis.Handler.Runs
    ( RunsConfig (..)
    , runsHandler
    )
where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BC
import Network.HTTP.Client
    ( BodyReader
    , Manager
    , Request (method, requestHeaders)
    , brRead
    , parseRequest
    , responseBody
    , responseHeaders
    , responseStatus
    , withResponse
    )
import Network.HTTP.Types
    ( Header
    , hAuthorization
    , hContentLength
    , hContentType
    , statusCode
    )
import Network.Wai
    ( Application
    , Request (rawQueryString)
    , responseLBS
    , responseStream
    )

data RunsConfig = RunsConfig
    { runsAntithesisUrl :: String
    , runsAntithesisApiKey :: ByteString
    , runsManager :: Manager
    }

runsHandler :: RunsConfig -> Application
runsHandler config request respond = do
    upstreamRequest <- parseRequest $ runsUrl config (rawQueryString request)
    withResponse
        upstreamRequest
            { method = "GET"
            , requestHeaders =
                [(hAuthorization, bearerAuthorization config)]
            }
        (runsManager config)
        $ \upstreamResponse ->
            if statusCode (responseStatus upstreamResponse) >= 500
                then
                    respond
                        $ responseLBS
                            (responseStatus upstreamResponse)
                            [(hContentType, "text/plain")]
                            "upstream request failed"
                else
                    respond
                        $ responseStream
                            (responseStatus upstreamResponse)
                            (forwardedHeaders $ responseHeaders upstreamResponse)
                            (streamBody $ responseBody upstreamResponse)

runsUrl :: RunsConfig -> ByteString -> String
runsUrl config query =
    stripTrailingSlash (runsAntithesisUrl config)
        <> "/api/v0/runs"
        <> BC.unpack query

bearerAuthorization :: RunsConfig -> ByteString
bearerAuthorization config =
    "Bearer " <> runsAntithesisApiKey config

forwardedHeaders :: [Header] -> [Header]
forwardedHeaders =
    filter
        ( \header ->
            fst header == hContentType
                || fst header == hContentLength
        )

stripTrailingSlash :: String -> String
stripTrailingSlash =
    reverse . dropWhile (== '/') . reverse

streamBody :: BodyReader -> (Builder.Builder -> IO ()) -> IO () -> IO ()
streamBody reader write flush = go
  where
    go = do
        chunk <- brRead reader
        unless (BS.null chunk) $ do
            write $ Builder.byteString chunk
            flush
            go
