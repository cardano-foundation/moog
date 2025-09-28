{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module Oracle.TypesSpec
    ( spec
    )
where

import Core.Types.Basic
    ( GithubUsername (GithubUsername)
    , Owner (..)
    , Platform (Platform)
    , PublicKeyHash (..)
    , RequestRefId (RequestRefId)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation (Operation (..))
import Core.Types.Tx (Root (..))
import Data.Functor.Identity (Identity (..))
import Oracle.Types
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Canonical (roundTrip)
import Test.QuickCheck
    ( Gen
    , Testable (..)
    , elements
    , forAll
    , listOf
    )
import Test.QuickCheck.Gen (oneof)
import User.Types
    ( RegisterUserKey (..)
    )

genChange
    :: Applicative f => f k -> f (Operation op) -> f (Change k op)
genChange genKey genValue = (Change . Key <$> genKey) <*> genValue

genRequest
    :: Applicative f => f k -> f (Operation op) -> f (Request k op)
genRequest genKey genValue =
    (Request . RequestRefId) "abc123" (Owner "ownerName")
        <$> genChange genKey genValue

genPlatform :: Gen Platform
genPlatform = elements [Platform "linux", Platform "windows", Platform "macos"]

genUsername :: Gen GithubUsername
genUsername = GithubUsername <$> listOf (elements ['a' .. 'z'])

genPublicKeyHash :: Gen PublicKeyHash
genPublicKeyHash = PublicKeyHash <$> listOf (elements $ ['a' .. 'f'] ++ ['0' .. '9'])

genRegisterUserKey :: Gen RegisterUserKey
genRegisterUserKey = do
    RegisterUserKey <$> genPlatform <*> genUsername <*> genPublicKeyHash

genRequestZoo :: Gen RequestZoo
genRequestZoo =
    oneof
        [ RegisterUserRequest
            <$> genRequest genRegisterUserKey (pure (Insert ()))
        , UnregisterUserRequest
            <$> genRequest genRegisterUserKey (pure (Delete ()))
        ]

genRefId :: Gen RequestRefId
genRefId = pure (RequestRefId "abc123")

genTokenState :: Gen TokenState
genTokenState =
    TokenState
        <$> pure (Root "rootHash")
        <*> pure (Owner "ownerName")

genToken :: Gen (Token Identity)
genToken = do
    Token
        <$> genRefId
        <*> genTokenState
        <*> (fmap Identity <$> listOf genRequestZoo)

spec :: Spec
spec = do
    describe "Change values" $ do
        it "roundtrips on the JSON instance"
            $ property
            $ forAll
                ( genChange
                    (listOf (elements ['a' .. 'z']))
                    (pure (Insert ()))
                )
            $ \change -> roundTrip change
    describe "Request values" $ do
        it "roundtrips on the JSON instance"
            $ property
            $ forAll genRequestZoo
            $ \request -> roundTrip request
    describe "Token values" $ do
        it "roundtrips on the JSON instance"
            $ property
            $ forAll genToken
            $ \token -> roundTrip token
