{-# LANGUAGE OverloadedStrings #-}

module Proxy.Antithesis.CacheSpec
    ( spec
    )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime (..)
    , addUTCTime
    , secondsToDiffTime
    )
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Lib.GitHub.Auth.TeamCheck (MembershipResult (..))
import Proxy.Antithesis.Cache
    ( cachedMembership
    , newMembershipCache
    , tokenPrefixHash
    )
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec =
    describe "Proxy.Antithesis.Cache" $ do
        it "caches membership results inside the configured TTL" $ do
            nowRef <- newIORef baseTime
            callsRef <- newIORef (0 :: Int)
            cache <- newMembershipCache (60 :: NominalDiffTime)

            first <-
                cachedMembership
                    cache
                    (readIORef nowRef)
                    token
                    (fetch callsRef Active)
            writeIORef nowRef $ addUTCTime 30 baseTime
            second <-
                cachedMembership
                    cache
                    (readIORef nowRef)
                    token
                    (fetch callsRef NotMember)

            calls <- readIORef callsRef
            first `shouldBe` Active
            second `shouldBe` Active
            calls `shouldBe` 1

        it "expires membership results after the configured TTL" $ do
            nowRef <- newIORef baseTime
            callsRef <- newIORef (0 :: Int)
            cache <- newMembershipCache (60 :: NominalDiffTime)

            first <-
                cachedMembership
                    cache
                    (readIORef nowRef)
                    token
                    (fetch callsRef Active)
            writeIORef nowRef $ addUTCTime 61 baseTime
            second <-
                cachedMembership
                    cache
                    (readIORef nowRef)
                    token
                    (fetch callsRef NotMember)

            calls <- readIORef callsRef
            first `shouldBe` Active
            second `shouldBe` NotMember
            calls `shouldBe` 2

        it "hashes only the first sixteen token bytes" $ do
            tokenPrefixHash (OAuthToken "abcdefghijklmnopAAAA")
                `shouldBe` tokenPrefixHash (OAuthToken "abcdefghijklmnopBBBB")
            tokenPrefixHash (OAuthToken "abcdefghijklmnopAAAA")
                `shouldNotBe` tokenPrefixHash (OAuthToken "abcdefghijklmnoXAAAA")

token :: OAuthToken
token = OAuthToken "abcdefghijklmnop-rest"

baseTime :: UTCTime
baseTime = UTCTime (toEnum 0) (secondsToDiffTime 0)

fetch :: IORef Int -> MembershipResult -> IO MembershipResult
fetch callsRef result = do
    calls <- readIORef callsRef
    writeIORef callsRef $ calls + 1
    pure result
