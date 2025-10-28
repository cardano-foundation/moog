{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module User.TypesSpec
    ( spec
    )
where

import Core.Types.Basic
    ( Commit (Commit)
    , Directory (Directory)
    , Duration (..)
    , GithubRepository (GithubRepository, organization, project)
    , GithubUsername (GithubUsername)
    , Platform (Platform)
    , Try (..)
    )
import Lib.SSH.Public (makeSSHPublicKey)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Canonical (roundTrip)
import Test.QuickCheck
    ( ASCIIString (..)
    , Gen
    , Testable (..)
    , elements
    , forAll
    , forAllBlind
    , listOf
    )
import Test.QuickCheck.Crypton (ed25519Gen)
import User.Types
    ( GithubIdentification (IdentifyViaSSHKey)
    , Outcome (OutcomeSuccess)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunRejection (..)
    , TestRunState (..)
    , URL (..)
    )

testRunRejectionGen :: Gen TestRunRejection
testRunRejectionGen = do
    elements
        [ BrokenInstructions
        , UnclearIntent
        ]

spec :: Spec
spec = do
    describe "TestRun" $ do
        it "roundtrips on the JSON instance" $ do
            let testRun =
                    TestRun
                        { platform = Platform "github"
                        , repository =
                            GithubRepository
                                { organization = "user"
                                , project = "repo"
                                }
                        , commitId = Commit "abc123"
                        , directory = Directory "src"
                        , requester = GithubUsername "tester"
                        , tryIndex = Try 1
                        }
            roundTrip testRun

    describe "TestRunState" $ do
        it "roundtrips on the JSON instance"
            $ property
            $ \message
               duration
               (ASCIIString url) -> forAll (listOf testRunRejectionGen) $ \rejections -> do
                    forAllBlind ed25519Gen $ \(sign, _verify) -> do
                        let pending = Pending (Duration duration) $ sign message
                        roundTrip pending
                        let rejected =
                                Rejected
                                    pending
                                    rejections
                        roundTrip rejected
                        let accepted = Accepted pending
                        roundTrip accepted
                        let finished =
                                Finished
                                    accepted
                                    (Duration duration)
                                    OutcomeSuccess
                                    (URL url)
                        roundTrip finished

    describe "RegisterUserKey" $ do
        it "roundtrips on the JSON instance" $ do
            let registerPubKey =
                    RegisterUserKey
                        { platform = Platform "github"
                        , username = GithubUsername "tester"
                        , githubIdentification =
                            IdentifyViaSSHKey
                                $ makeSSHPublicKey
                                    "AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8"
                        }
            roundTrip registerPubKey
