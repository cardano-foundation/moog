{-# LANGUAGE OverloadedStrings #-}

-- | Handler for proxying Antithesis run listings.
module Proxy.Antithesis.Handler.Runs
    ( RunsConfig (..)
    , runsHandler
    )
where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
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
    , runsAntithesisUser :: ByteString
    , runsAntithesisPassword :: ByteString
    , runsManager :: Manager
    }

runsHandler :: RunsConfig -> Application
runsHandler config request respond = do
    upstreamRequest <- parseRequest $ runsUrl config (rawQueryString request)
    withResponse
        upstreamRequest
            { method = "GET"
            , requestHeaders =
                [(hAuthorization, basicAuthorization config)]
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
        <> "/api/v1/runs"
        <> BC.unpack query

basicAuthorization :: RunsConfig -> ByteString
basicAuthorization config =
    "Basic "
        <> Base64.encode
            (runsAntithesisUser config <> ":" <> runsAntithesisPassword config)

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
