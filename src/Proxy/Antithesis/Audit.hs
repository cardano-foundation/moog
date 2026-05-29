{-# LANGUAGE OverloadedStrings #-}

-- | JSON audit-line rendering for proxy requests.
module Proxy.Antithesis.Audit
    ( AuditEvent (..)
    , RequestId (..)
    , renderAuditEvent
    )
where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format
    ( defaultTimeLocale
    , formatTime
    )
import Lib.GitHub.Auth.Identify (Login)
import Network.HTTP.Types (Status, statusCode)

newtype RequestId = RequestId Text
    deriving stock (Eq, Show)

data AuditEvent = AuditEvent
    { auditTimestamp :: UTCTime
    , auditLogin :: Maybe Login
    , auditPath :: Text
    , auditStatus :: Status
    , auditUpstreamStatus :: Maybe Status
    , auditLatencyMs :: Int
    , auditRequestId :: RequestId
    }
    deriving stock (Eq, Show)

renderAuditEvent :: AuditEvent -> LBS.ByteString
renderAuditEvent event =
    encode
        ( object
            [ "ts" .= renderTimestamp (auditTimestamp event)
            , "login" .= auditLogin event
            , "path" .= auditPath event
            , "status" .= statusCode (auditStatus event)
            , "upstream_status" .= fmap statusCode (auditUpstreamStatus event)
            , "latency_ms" .= auditLatencyMs event
            , "request_id" .= unRequestId (auditRequestId event)
            ]
        )
        <> "\n"

unRequestId :: RequestId -> Text
unRequestId (RequestId requestId) = requestId

renderTimestamp :: UTCTime -> Text
renderTimestamp =
    T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
