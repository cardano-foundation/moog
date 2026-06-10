{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A mock of the Antithesis read API (the @GET \/api\/v0\/runs@ shape
-- from "Proxy.Antithesis.Api") served as a Warp test server, so the
-- agent's reconcile→report cycle can be exercised without a live
-- Antithesis tenant.
--
-- The served run list is supplied by an @IO [AntithesisRun]@ action read
-- on every request, so a test may mutate an 'Data.IORef.IORef' between
-- reconcile cycles to drive Pending→Running→Done transitions (slices
-- B\/C). Pagination follows the real upstream contract: @limit@ caps the
-- page size and @after@ carries the previous page's last run id as an
-- opaque cursor; @next_cursor@ is absent on the final page. A missing or
-- non-@Bearer@ @Authorization@ header is rejected with @401@.
module Lib.Agent.MockAntithesis
    ( withMockAntithesisServer
    )
where

import Control.Monad (join)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types
    ( hAuthorization
    , queryToQueryText
    , status200
    , status401
    )
import Network.Wai
    ( Application
    , Request (queryString, requestHeaders)
    , responseLBS
    )
import Network.Wai.Handler.Warp (testWithApplication)
import Text.Read (readMaybe)
import User.Agent.Antithesis.State
    ( AntithesisRun (..)
    , AntithesisRunStatus (..)
    )

-- | Stand up the mock Antithesis read API on a fresh ephemeral port and
-- run the action with its base URL (e.g. @http:\/\/127.0.0.1:PORT@). The
-- run list is re-read from the provider on each request so a test can
-- evolve it across reconcile cycles.
withMockAntithesisServer
    :: IO [AntithesisRun]
    -- ^ run-list provider, consulted per request
    -> (String -> IO a)
    -- ^ action given the server base URL
    -> IO a
withMockAntithesisServer getRuns action =
    testWithApplication (pure $ runsApp getRuns) $ \port ->
        action $ "http://127.0.0.1:" <> show port

-- | The WAI application: authenticate the @Bearer@ token, then serve one
-- page of the current run list honouring the @limit@\/@after@ query.
runsApp :: IO [AntithesisRun] -> Application
runsApp getRuns request respond =
    case lookup hAuthorization (requestHeaders request) of
        Just auth
            | "Bearer " `BS.isPrefixOf` auth -> do
                runs <- getRuns
                let queryText = queryToQueryText (queryString request)
                    after = join (lookup "after" queryText)
                    limit =
                        readLimit =<< join (lookup "limit" queryText)
                respond $
                    responseLBS
                        status200
                        [("Content-Type", "application/json")]
                        (Aeson.encode (runsPage runs limit after))
        _ -> respond $ responseLBS status401 [] ""

readLimit :: Text -> Maybe Int
readLimit = readMaybe . T.unpack

-- | One page of @runs@ after applying the @after@ cursor and @limit@.
-- The cursor is the previous page's last run id; @next_cursor@ is set
-- only when runs remain beyond the page just served.
runsPage :: [AntithesisRun] -> Maybe Int -> Maybe Text -> Aeson.Value
runsPage runs limit after =
    let afterRuns = case after of
            Nothing -> runs
            Just cursor ->
                drop 1 $
                    dropWhile ((/= cursor) . antithesisRunId) runs
        (page, remaining) =
            maybe (afterRuns, []) (`splitAt` afterRuns) limit
        nextCursor = case (remaining, reverse page) of
            (_ : _, lastRun : _) -> Just (antithesisRunId lastRun)
            _ -> Nothing
     in runsPageJson (map runToJson page) nextCursor

runsPageJson :: [Aeson.Value] -> Maybe Text -> Aeson.Value
runsPageJson runs nextCursor =
    Aeson.object
        [ "data" Aeson..= runs
        , "next_cursor" Aeson..= nextCursor
        ]

-- | Serialize an 'AntithesisRun' to the upstream wire shape that
-- 'User.Agent.Antithesis.State.parseRunsPage' consumes. Absent optional
-- fields are omitted (description) or rendered @null@ (links) so the
-- value round-trips back to the same 'AntithesisRun'.
runToJson :: AntithesisRun -> Aeson.Value
runToJson run =
    Aeson.object
        [ "run_id" Aeson..= antithesisRunId run
        , "status" Aeson..= statusText (antithesisRunStatus run)
        , "parameters"
            Aeson..= Aeson.object
                (descriptionField (antithesisRunDescription run))
        , "links"
            Aeson..= maybe
                Aeson.Null
                reportLinks
                (antithesisRunTriageReport run)
        ]
  where
    descriptionField Nothing = []
    descriptionField (Just description) =
        ["antithesis.description" Aeson..= description]
    reportLinks url = Aeson.object ["triage_report" Aeson..= url]

statusText :: AntithesisRunStatus -> Text
statusText = \case
    RunStarting -> "starting"
    RunInProgress -> "in_progress"
    RunCompleted -> "completed"
    RunCancelled -> "cancelled"
    RunIncomplete -> "incomplete"
    RunUnknown -> "unknown"
