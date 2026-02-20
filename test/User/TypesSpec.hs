{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module User.TypesSpec
    ( spec
    )
where

import Core.Types.Basic
    ( Commit (Commit)
    , Directory (Directory)
    , FaultsEnabled (..)
    , GithubRepository (GithubRepository, organization, project)
    , GithubUsername (GithubUsername)
    , Platform (Platform)
    , Try (..)
    )
import Core.Types.Duration (Duration (..))
import Data.ByteString.Lazy qualified as BL
import Lib.SSH.Public (makeSSHPublicKey)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
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
import Text.JSON.Canonical (FromJSON (..), parseCanonicalJSON)
import User.Types
    ( GithubIdentification (IdentifyViaSSHKey)
    , Outcome (..)
    , Phase (..)
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
               faultsEnabled
               (ASCIIString url) -> forAll (listOf testRunRejectionGen) $ \rejections -> do
                    forAllBlind ed25519Gen $ \(sign, _verify) -> do
                        let pending =
                                Pending
                                    (Hours duration)
                                    (FaultsEnabled faultsEnabled)
                                    $ sign message
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
                                    (Hours duration)
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

    -- Golden tests using real on-chain data from the moog token.
    -- These prove that the current code can parse test-run facts
    -- written by older moog versions (duration as plain number).
    describe "Golden on-chain facts (old duration format)" $ do
        let parseFact
                :: BL.ByteString
                -> BL.ByteString
                -> Either String (TestRun, TestRunState DoneT)
            parseFact keyJson valueJson = do
                kv <- parseCanonicalJSON keyJson
                vv <- parseCanonicalJSON valueJson
                case (fromJSON kv, fromJSON vv) of
                    (Just k, Just v) -> Right (k, v)
                    (Nothing, _) -> Left "failed to parse TestRun key"
                    (_, Nothing) -> Left "failed to parse TestRunState value"

        it "parses finished/unknown test-run (duration:1)" $ do
            case
                parseFact
                    "{\"commitId\":\"031bff3ab7dd23158b94eb377cdbd6be575566e1\",\"directory\":\"testnets/cardano_node_master\",\"platform\":\"github\",\"repository\":{\"organization\":\"cardano-foundation\",\"repo\":\"cardano-node-antithesis\"},\"requester\":\"cfhal\",\"try\":1,\"type\":\"test-run\"}"
                    "{\"duration\":1,\"from\":{\"from\":{\"duration\":1,\"faults_enabled\":true,\"phase\":\"pending\",\"signature\":\"bb5dcd3b9fe400e6afcbaefc25eda815fa23186b01e376a712867aa89aad0b0eec49b15115155751e6c1faa35d97cf31d2d70b1932c73c04c237090f0b24d00f\"},\"phase\":\"accepted\"},\"outcome\":\"unknown\",\"phase\":\"finished\",\"url\":\"encrypted-url-data\"}"
                of
                    Left err -> expectationFailure err
                    Right (testRun, done) -> do
                        commitId testRun `shouldBe` Commit "031bff3ab7dd23158b94eb377cdbd6be575566e1"
                        tryIndex testRun `shouldBe` Try 1
                        case done of
                            Finished _ dur outcome _ -> do
                                dur `shouldBe` Hours 1
                                outcome `shouldBe` OutcomeUnknown
                            _ -> expectationFailure "expected Finished state"

        it "parses finished/failure test-run (duration:1)" $ do
            case
                parseFact
                    "{\"commitId\":\"03e14eab93d2bc008e1be4deb75430c1a8d98948\",\"directory\":\"compose/testnets/cardano_node_master\",\"platform\":\"github\",\"repository\":{\"organization\":\"cardano-foundation\",\"repo\":\"moog\"},\"requester\":\"cfhal\",\"try\":1,\"type\":\"test-run\"}"
                    "{\"duration\":1,\"from\":{\"from\":{\"duration\":1,\"faults_enabled\":true,\"phase\":\"pending\",\"signature\":\"416ed1bf5c531674ed22511aeefeadb935d79691e03e16a9c3771a44fc64b6e558ee61d8266023fcee10d8042583c7fef919b64d7e295405d64f55f6bb855104\"},\"phase\":\"accepted\"},\"outcome\":\"failure\",\"phase\":\"finished\",\"url\":\"encrypted-url-data\"}"
                of
                    Left err -> expectationFailure err
                    Right (testRun, done) -> do
                        commitId testRun `shouldBe` Commit "03e14eab93d2bc008e1be4deb75430c1a8d98948"
                        case done of
                            Finished _ dur outcome _ -> do
                                dur `shouldBe` Hours 1
                                outcome `shouldBe` OutcomeFailure
                            _ -> expectationFailure "expected Finished state"

        it "parses finished/success test-run (duration:1, faults_enabled:false)" $ do
            case
                parseFact
                    "{\"commitId\":\"10b3f601e82560c182f08ba4070f20aab934a0e9\",\"directory\":\"compose/testnets/cardano_node_master\",\"platform\":\"github\",\"repository\":{\"organization\":\"cardano-foundation\",\"repo\":\"moog\"},\"requester\":\"cfhal\",\"try\":2,\"type\":\"test-run\"}"
                    "{\"duration\":1,\"from\":{\"from\":{\"duration\":1,\"faults_enabled\":false,\"phase\":\"pending\",\"signature\":\"483d4933bba2eea6496cf5dbb28288f5a0076f685714a908c2ac24b625141c642caa504ea9721119875c56caef08f00c0af119b39fbb0fb2bb71a99cb2b42e06\"},\"phase\":\"accepted\"},\"outcome\":\"success\",\"phase\":\"finished\",\"url\":\"encrypted-url-data\"}"
                of
                    Left err -> expectationFailure err
                    Right (testRun, done) -> do
                        tryIndex testRun `shouldBe` Try 2
                        case done of
                            Finished _ dur outcome _ -> do
                                dur `shouldBe` Hours 1
                                outcome `shouldBe` OutcomeSuccess
                            _ -> expectationFailure "expected Finished state"

        it "parses finished test-run with duration:3" $ do
            case
                parseFact
                    "{\"commitId\":\"071b965acee2b8b7529df4eb5a1e87d525d29ba0\",\"directory\":\"testnets/cardano_node_master\",\"platform\":\"github\",\"repository\":{\"organization\":\"cardano-foundation\",\"repo\":\"cardano-node-antithesis\"},\"requester\":\"cfhal\",\"try\":1,\"type\":\"test-run\"}"
                    "{\"duration\":3,\"from\":{\"from\":{\"duration\":3,\"faults_enabled\":true,\"phase\":\"pending\",\"signature\":\"3c5ecd80f97792efed818c3c066df4522af3f6e2514943b91863c31badccaa18d01de81e381417f40c7dc27592341e74222c5abe151bd7a369204d13025a1005\"},\"phase\":\"accepted\"},\"outcome\":\"unknown\",\"phase\":\"finished\",\"url\":\"encrypted-url-data\"}"
                of
                    Left err -> expectationFailure err
                    Right (_testRun, done) -> do
                        case done of
                            Finished _ dur _ _ -> dur `shouldBe` Hours 3
                            _ -> expectationFailure "expected Finished state"
