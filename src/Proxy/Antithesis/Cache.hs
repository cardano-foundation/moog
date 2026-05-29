{-# LANGUAGE OverloadedStrings #-}

-- | Short-lived membership cache for GitHub token authorization.
module Proxy.Antithesis.Cache
    ( MembershipCache
    , TokenPrefixHash
    , cachedMembership
    , newMembershipCache
    , tokenPrefixHash
    )
where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteString.Char8 qualified as BC
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Lib.GitHub.Auth.TeamCheck (MembershipResult)

data MembershipCache = MembershipCache
    { cacheTtl :: NominalDiffTime
    , cacheEntries :: IORef (Map TokenPrefixHash (UTCTime, MembershipResult))
    }

newtype TokenPrefixHash = TokenPrefixHash Text
    deriving stock (Eq, Show)
    deriving newtype (Ord)

newMembershipCache :: NominalDiffTime -> IO MembershipCache
newMembershipCache ttl =
    MembershipCache ttl <$> newIORef Map.empty

cachedMembership
    :: MembershipCache
    -> IO UTCTime
    -> OAuthToken
    -> IO MembershipResult
    -> IO MembershipResult
cachedMembership cache getNow token fetchMembership = do
    now <- getNow
    entries <- readIORef $ cacheEntries cache
    case Map.lookup key entries of
        Just (insertedAt, result)
            | diffUTCTime now insertedAt <= cacheTtl cache ->
                pure result
        _ -> do
            result <- fetchMembership
            atomicModifyIORef'
                (cacheEntries cache)
                ( \oldEntries ->
                    (Map.insert key (now, result) oldEntries, ())
                )
            pure result
  where
    key = tokenPrefixHash token

tokenPrefixHash :: OAuthToken -> TokenPrefixHash
tokenPrefixHash (OAuthToken token) =
    TokenPrefixHash
        $ T.pack
        $ show (hash (BC.take 16 token) :: Digest SHA256)
