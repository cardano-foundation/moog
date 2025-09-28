module User.Agent.PushTestSpec
    ( spec
    )
where

import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , GithubRepository (..)
    , GithubUsername (..)
    , Platform (..)
    , Try (..)
    )
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import User.Agent.PushTest
    ( AntithesisAuth (..)
    , PostTestRunRequest (..)
    , Registry (..)
    , Tag (..)
    , buildConfigImage
    , collectImagesFromAssets
    , renderPostToAntithesis
    , renderTestRun
    )
import User.Agent.Types (TestRunId (..))
import User.Types (TestRun (..))

spec :: Spec
spec = do
    describe "collectImagesFromAssets" $ do
        it "should collect images from a docker-compose file" $ do
            -- Here you would write tests for the collectImagesFromAssets function
            collectImagesFromAssets (Directory "test/data")
                `shouldReturn` Right
                    [ "ghcr.io/cardano-foundation/antithesis/configurator:latest"
                    , "ghcr.io/cardano-foundation/antithesis/sidecar:latest"
                    , "ghcr.io/cardano-foundation/antithesis/tracer-sidecar:latest"
                    , "ghcr.io/cardano-foundation/antithesis/tracer:latest"
                    , "ghcr.io/intersectmbo/cardano-node:latest"
                    ]
    describe "buildConfigImage" $ do
        it "should build the Dockerfile for the cardano_node_master" $ do
            buildConfigImage
                (Registry "registry")
                (Directory "test/data")
                (TestRunId "dummy")
                `shouldReturn` Right
                    (Tag "registry/cardano-anti-cli-config:dummy")
    describe "renderPostToAntithesis" $ do
        it "should render the curl command for pushing to Antithesis" $ do
            Right images <-
                collectImagesFromAssets (Directory "test/data")
            Right configTag <-
                buildConfigImage
                    (Registry "registry")
                    (Directory "test/data")
                    (TestRunId "dummy")
            let
                testRun =
                    TestRun
                        { platform = Platform "github"
                        , repository = GithubRepository "cardano-foundation" "antithesis"
                        , directory = Directory "tests"
                        , commitId = Commit "abcdef1234567890"
                        , tryIndex = Try 1
                        , requester = GithubUsername "alice"
                        }
                testRunId = TestRunId "test-run-001"
                auth = AntithesisAuth "user" "pass"
                body =
                    PostTestRunRequest
                        { description = renderTestRun testRunId testRun
                        , duration = 3600
                        , config_image = tagString configTag
                        , images = images
                        , recipients = ["hal@cardanofoundation.org"]
                        , source = "dummy"
                        , slack = Nothing
                        }
                (cmd, args) = renderPostToAntithesis auth body
            cmd `shouldBe` "curl"
            args
                `shouldBe` [ "--fail"
                           , "-u"
                           , "user:pass"
                           , "-X"
                           , "POST"
                           , "https://cardano.antithesis.com/api/v1/launch/cardano"
                           , "-H"
                           , "Content-Type: application/json"
                           , "-d"
                           , "{\"params\":{\"antithesis.config_image\":\"registry/cardano-anti-cli-config:dummy\",\"antithesis.description\":\"{\\\"testRun\\\":{\\\"commitId\\\":\\\"abcdef1234567890\\\",\\\"directory\\\":\\\"tests\\\",\\\"platform\\\":\\\"github\\\",\\\"repository\\\":{\\\"organization\\\":\\\"cardano-foundation\\\",\\\"repo\\\":\\\"antithesis\\\"},\\\"requester\\\":\\\"alice\\\",\\\"try\\\":1,\\\"type\\\":\\\"test-run\\\"},\\\"testRunId\\\":\\\"test-run-001\\\"}\",\"antithesis.duration\":3600,\"antithesis.images\":\"ghcr.io/cardano-foundation/antithesis/configurator:latest;ghcr.io/cardano-foundation/antithesis/sidecar:latest;ghcr.io/cardano-foundation/antithesis/tracer-sidecar:latest;ghcr.io/cardano-foundation/antithesis/tracer:latest;ghcr.io/intersectmbo/cardano-node:latest\",\"antithesis.report.recipients\":\"hal@cardanofoundation.org\",\"antithesis.source\":\"dummy\"}}"
                           ]
