{-# LANGUAGE OverloadedStrings #-}

module User.Agent.ProcessOptionsSpec
    ( spec
    )
where

import Control.Exception (bracket, bracket_, try)
import Core.Types.Basic (TokenId (..))
import Data.List (isPrefixOf)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Environment
    ( lookupEnv
    , setEnv
    , unsetEnv
    , withArgs
    )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO
    ( IOMode (..)
    , hClose
    , stderr
    , withFile
    )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    )
import User.Agent.Process
    ( ProcessOptions (..)
    , parseArgs
    )
import User.Agent.PublishResults.Email (Minutes (..))
import User.Agent.PushTest
    ( AntithesisAuth (..)
    , LaunchUrl (..)
    , Registry (..)
    )

-- | Names of every environment variable that the agent parser reads.
-- The list is used to scrub the environment for hermetic tests so that
-- a caller's shell cannot bleed values into the parser under test.
agentEnvVars :: [String]
agentEnvVars =
    [ "MOOG_SECRETS_FILE"
    , "MOOG_INTERACTIVE_SECRETS"
    , "MOOG_GITHUB_PAT"
    , "MOOG_WALLET_FILE"
    , "MOOG_TOKEN_ID"
    , "MOOG_MPFS_HOST"
    , "MOOG_WAIT"
    , "MOOG_MPFS_TIMEOUT_SECONDS"
    , "POLL_INTERVAL_SECONDS"
    , "MOOG_AGENT_EMAIL"
    , "MOOG_AGENT_EMAIL_PASSWORD"
    , "MOOG_ANTITHESIS_USER"
    , "MOOG_ANTITHESIS_PASSWORD"
    , "MOOG_ANTITHESIS_LAUNCH_URL"
    , "MOOG_REGISTRY"
    , "MOOG_SLACK_WEBHOOK"
    , "MOOG_WALLET_PASSPHRASE"
    ]

withScrubbedEnv :: IO a -> IO a
withScrubbedEnv = bracket_ (saveAndUnset agentEnvVars) restoreAll
  where
    saveAndUnset xs = mapM_ unsetEnv xs >> pure ()
    restoreAll = pure ()

-- | Redirect stderr to /dev/null for the duration of the action. Used to
-- suppress the OptEnvConf error renderer when a test deliberately triggers
-- a parse failure.
silenceStderr :: IO a -> IO a
silenceStderr action =
    withFile "/dev/null" WriteMode $ \devNull ->
        bracket
            (hDuplicate stderr)
            (\saved -> hDuplicateTo saved stderr >> hClose saved)
            (\_ -> hDuplicateTo devNull stderr >> action)

-- | Temporarily set an env var inside an IO action, restoring whatever
-- value (or absence) the variable had before.
withEnv :: String -> String -> IO a -> IO a
withEnv name v action = do
    mPrev <- lookupEnv name
    bracket_ (setEnv name v) (restore mPrev) action
  where
    restore Nothing = unsetEnv name
    restore (Just old) = setEnv name old

walletJsonContent :: String
walletJsonContent =
    "{\"mnemonics\":\"coin april solid purity wish slight \
    \acquire kitchen dragon faculty clutch picnic\"}"

agentYamlContent :: FilePath -> String
agentYamlContent walletPath =
    unlines
        [ "githubPAT: test-github-token"
        , "walletFile: " ++ walletPath
        , "tokenId: yaml-token"
        , "mpfsHost: http://127.0.0.1:38081"
        , "wait: 9"
        , "mpfsTimeoutSeconds: 9"
        , "pollIntervalSeconds: 7"
        , "minutes: 11"
        , "registry: registry.example.invalid/moog"
        , "antithesisUser: ant-user"
        , "antithesisPassword: ant-pass"
        , "antithesisLaunchUrl: https://example.invalid/api/v1/launch/cardano"
        , "agentEmail: agent@example.invalid"
        , "agentEmailPassword: email-pass"
        , "trustedRequesters:"
        , "  - alice"
        ]

-- | Variant of 'agentYamlContent' with the @antithesisLaunchUrl@ key removed,
-- so the parser sees no source for the tenant launch URL.
agentYamlContentWithoutLaunchUrl :: FilePath -> String
agentYamlContentWithoutLaunchUrl walletPath =
    unlines
        . filter (not . isPrefixOf "antithesisLaunchUrl")
        . lines
        $ agentYamlContent walletPath

writeFixtures :: FilePath -> IO (FilePath, FilePath)
writeFixtures tmp = do
    let walletPath = tmp </> "agent-wallet.json"
        yamlPath = tmp </> "agent.yaml"
    writeFile walletPath walletJsonContent
    writeFile yamlPath (agentYamlContent walletPath)
    pure (walletPath, yamlPath)

withParsedAgent
    :: [String]
    -- ^ extra command-line arguments
    -> (ProcessOptions -> IO ())
    -> IO ()
withParsedAgent extraArgs k =
    withScrubbedEnv $ withSystemTempDirectory "moog-agent-conf" $ \tmp -> do
        (_walletPath, yamlPath) <- writeFixtures tmp
        let args =
                [ "--secrets-file"
                , yamlPath
                , "--trust-all-requesters"
                ]
                    ++ extraArgs
        opts <- withArgs args parseArgs
        k opts

spec :: Spec
spec = describe "agent file-backed configuration" $ do
    it "loads runtime settings from a YAML secrets file" $ do
        withParsedAgent [] $ \opts -> do
            poTokenId opts `shouldBe` TokenId "yaml-token"
            poPollIntervalSeconds opts `shouldBe` 7
            poMinutes opts `shouldBe` Minutes 11
            poRegistry opts `shouldBe` Registry "registry.example.invalid/moog"
            poAntithesisAuth opts
                `shouldBe` AntithesisAuth
                    { username = "ant-user"
                    , password = "ant-pass"
                    , launchUrl =
                        LaunchUrl
                            "https://example.invalid/api/v1/launch/cardano"
                    }
    it "lets MOOG_TOKEN_ID override the YAML tokenId value" $ do
        withScrubbedEnv $ withEnv "MOOG_TOKEN_ID" "env-token" $ do
            withSystemTempDirectory "moog-agent-conf-prec" $ \tmp -> do
                (_walletPath, yamlPath) <- writeFixtures tmp
                let args =
                        [ "--secrets-file"
                        , yamlPath
                        , "--trust-all-requesters"
                        ]
                opts <- withArgs args parseArgs
                poTokenId opts `shouldBe` TokenId "env-token"
    it "fails closed when no launch URL is supplied" $ do
        withScrubbedEnv
            $ withSystemTempDirectory "moog-agent-conf-no-url"
            $ \tmp -> do
                let walletPath = tmp </> "agent-wallet.json"
                    yamlPath = tmp </> "agent.yaml"
                writeFile walletPath walletJsonContent
                writeFile
                    yamlPath
                    (agentYamlContentWithoutLaunchUrl walletPath)
                let args =
                        [ "--secrets-file"
                        , yamlPath
                        , "--trust-all-requesters"
                        ]
                result <-
                    try $ silenceStderr $ withArgs args parseArgs
                case result of
                    Left (ExitFailure _) -> pure ()
                    Left ExitSuccess ->
                        expectationFailure
                            "parser returned ExitSuccess; expected\
                            \ ExitFailure for missing launch URL"
                    Right _ ->
                        expectationFailure
                            "parser succeeded; expected ExitFailure\
                            \ when antithesisLaunchUrl is unset"
