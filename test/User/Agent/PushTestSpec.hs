module User.Agent.PushTestSpec
    ( spec
    )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text
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
    , LaunchUrl (..)
    , PostTestRunRequest (..)
    , Registry (..)
    , Tag (..)
    , buildConfigImage
    , renderPostToAntithesis
    , renderTestRun
    )
import User.Agent.Types (TestRunId (..))
import User.Types (TestRun (..))

spec :: Spec
spec = do
    describe "buildConfigImage" $ do
        it "should build the Dockerfile for the cardano_node_master" $ do
            buildConfigImage
                (Registry "registry")
                (Directory "test/data")
                (TestRunId "dummy")
                `shouldReturn` Right
                    (Tag "registry/cardano-moog-config:dummy")
    describe "renderPostToAntithesis" $ do
        it "should render the curl command for pushing to Antithesis" $ do
            Right configTag <-
                buildConfigImage
                    (Registry "registry")
                    (Directory "test/data")
                    (TestRunId "dummy")
            let
                testRun =
                    TestRun
                        { platform = Platform "github"
                        , repository = GithubRepository "cardano-foundation" "moog"
                        , directory = Directory "tests"
                        , commitId = Commit "abcdef1234567890"
                        , tryIndex = Try 1
                        , requester = GithubUsername "alice"
                        }
                testRunId = TestRunId "test-run-001"
                launchUrl =
                    "https://example.antithesis.com/api/v1/launch/cardano"
                auth =
                    AntithesisAuth "user" "pass" (LaunchUrl launchUrl)
                body =
                    PostTestRunRequest
                        { description = renderTestRun testRunId testRun
                        , duration = 3600
                        , config_image = tagString configTag
                        , recipients = ["hal@cardanofoundation.org"]
                        , source = "dummy"
                        , slack = Nothing
                        , faults_enabled = True
                        , is_haskell = False
                        , networkFaultExclusion = []
                        , containerFaultsKillExclusion = []
                        , containerFaultsPauseExclusion = []
                        , containerFaultsStopExclusion = []
                        }
                (cmd, args) = renderPostToAntithesis auth body
            cmd `shouldBe` "curl"
            args
                `shouldBe` [ "--fail"
                           , "-u"
                           , "user:pass"
                           , "-X"
                           , "POST"
                           , launchUrl
                           , "-H"
                           , "Content-Type: application/json"
                           , "-d"
                           , "{\"params\":{\"antithesis.config_image\":\"registry/cardano-moog-config:dummy\",\"antithesis.description\":\"{\\\"testRun\\\":{\\\"commitId\\\":\\\"abcdef1234567890\\\",\\\"directory\\\":\\\"tests\\\",\\\"platform\\\":\\\"github\\\",\\\"repository\\\":{\\\"organization\\\":\\\"cardano-foundation\\\",\\\"repo\\\":\\\"moog\\\"},\\\"requester\\\":\\\"alice\\\",\\\"try\\\":1,\\\"type\\\":\\\"test-run\\\"},\\\"testRunId\\\":\\\"test-run-001\\\"}\",\"antithesis.duration\":3600,\"antithesis.report.recipients\":\"hal@cardanofoundation.org\",\"antithesis.source\":\"dummy\",\"custom.faults_enabled\":true,\"custom.is_haskell\":false}}"
                           ]
    describe "PostTestRunRequest exclusion serialization" $ do
        it "omits exclusion keys when all exclusion lists are empty" $ do
            exclusionParams basePostTestRunRequest
                `shouldBe` [ Nothing
                           , Nothing
                           , Nothing
                           , Nothing
                           ]

        it "serializes only the non-empty kill exclusion list" $ do
            let request =
                    basePostTestRunRequest
                        { containerFaultsKillExclusion =
                            ["asteria-game", "tx-generator"]
                        }
            exclusionParams request
                `shouldBe` [ Nothing
                           , Just
                                $ Aeson.String
                                $ Text.pack "asteria-game,tx-generator"
                           , Nothing
                           , Nothing
                           ]

        it "serializes all exclusion lists independently" $ do
            let request =
                    basePostTestRunRequest
                        { networkFaultExclusion = ["net-a", "net-b"]
                        , containerFaultsKillExclusion = ["kill-a"]
                        , containerFaultsPauseExclusion =
                            ["pause-a", "pause-b"]
                        , containerFaultsStopExclusion = ["stop-a"]
                        }
            exclusionParams request
                `shouldBe` [ Just $ Aeson.String $ Text.pack "net-a,net-b"
                           , Just $ Aeson.String $ Text.pack "kill-a"
                           , Just
                                $ Aeson.String
                                $ Text.pack "pause-a,pause-b"
                           , Just $ Aeson.String $ Text.pack "stop-a"
                           ]

basePostTestRunRequest :: PostTestRunRequest
basePostTestRunRequest =
    PostTestRunRequest
        { description = "description"
        , duration = 3600
        , config_image = "registry/cardano-moog-config:dummy"
        , recipients = ["hal@cardanofoundation.org"]
        , source = "dummy"
        , slack = Nothing
        , faults_enabled = True
        , is_haskell = False
        , networkFaultExclusion = []
        , containerFaultsKillExclusion = []
        , containerFaultsPauseExclusion = []
        , containerFaultsStopExclusion = []
        }

exclusionParams :: PostTestRunRequest -> [Maybe Aeson.Value]
exclusionParams request =
    fmap
        (`lookupParam` request)
        [ "custom.network_fault_exclusion"
        , "custom.container_faults_kill_exclusion"
        , "custom.container_faults_pause_exclusion"
        , "custom.container_faults_stop_exclusion"
        ]

lookupParam :: String -> PostTestRunRequest -> Maybe Aeson.Value
lookupParam name request = do
    Aeson.Object topLevel <- Aeson.decode $ Aeson.encode request
    Aeson.Object params <- KeyMap.lookup (Key.fromString "params") topLevel
    KeyMap.lookup (Key.fromString name) params
