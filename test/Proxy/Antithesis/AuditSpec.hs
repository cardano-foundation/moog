{-# LANGUAGE OverloadedStrings #-}

module Proxy.Antithesis.AuditSpec
    ( spec
    )
where

import Data.Aeson
    ( FromJSON (parseJSON)
    , Value (..)
    , eitherDecode
    , withObject
    , (.:)
    )
import Data.ByteString.Lazy.Char8 qualified as LBC
import Data.List qualified as List
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Lib.GitHub.Auth.Identify (Login (..))
import Network.HTTP.Types (status200)
import Proxy.Antithesis.Audit
    ( AuditEvent (..)
    , RequestId (..)
    , renderAuditEvent
    )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
    describe "Proxy.Antithesis.Audit" $
        it "renders a sanitized JSON line with correlation fields" $ do
            let rendered =
                    renderAuditEvent
                        AuditEvent
                            { auditTimestamp = baseTime
                            , auditLogin = Just (Login "octocat")
                            , auditPath = "/api/v1/runs"
                            , auditStatus = status200
                            , auditUpstreamStatus = Just status200
                            , auditLatencyMs = 12
                            , auditRequestId = RequestId "req-123"
                            }

            rendered `shouldSatisfy` LBC.isSuffixOf "\n"
            decoded rendered
                `shouldBe` Right
                    ( "octocat"
                    , "/api/v1/runs"
                    , 200
                    , Just 200
                    , 12
                    , "req-123"
                    )
            rendered `shouldSatisfy` not . contains "secret-token"
            rendered `shouldSatisfy` not . contains "upstream-body"

baseTime :: UTCTime
baseTime = UTCTime (toEnum 0) (secondsToDiffTime 0)

newtype AuditTuple = AuditTuple
    (Text, Text, Int, Maybe Int, Int, Text)
    deriving stock (Eq, Show)

instance FromJSON AuditTuple where
    parseJSON =
        withObject "AuditTuple" $ \o ->
            AuditTuple
                <$> ( (,,,,,)
                        <$> o .: "login"
                        <*> o .: "path"
                        <*> o .: "status"
                        <*> o .: "upstream_status"
                        <*> o .: "latency_ms"
                        <*> o .: "request_id"
                    )

decoded :: LBC.ByteString -> Either String (Text, Text, Int, Maybe Int, Int, Text)
decoded raw = do
    AuditTuple tuple <- eitherDecode raw
    pure tuple

contains :: LBC.ByteString -> LBC.ByteString -> Bool
contains needle haystack =
    LBC.unpack needle `List.isInfixOf` LBC.unpack haystack
