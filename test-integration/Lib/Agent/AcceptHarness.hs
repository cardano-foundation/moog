{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Devnet-MPFS harness for the agent Pending→Running (Accept)
-- integration test. It boots a fresh token against the self-hosted
-- @mpfs-devnet-server@ (env @MOOG_MPFS_HOST@, role wallets via
-- @MOOG_TEST_*_WALLET@), seeds the on-chain prerequisites for an agent
-- Accept, and exposes the real agent\/oracle code paths needed to drive
-- and observe the transition.
--
-- The Accept validation only requires a token 'Config' fact (whose
-- @configAgent@ is the agent owner) and a Pending test-run fact; the
-- Pending signature is not verified on Accept. A valid Pending fact
-- cannot be produced hermetically through the normal create flow
-- (`validateCreateTestRunCore` makes a live GitHub commit check and
-- verifies a registered-key signature), so those two facts are
-- **seeded directly**: an MPFS insert request is folded into the token
-- via 'mpfsUpdateTokenFromFacts' signed by the oracle, bypassing moog's
-- off-chain request validation. The agent Accept and the oracle fold of
-- the resulting Accept request then exercise the real validated paths.
module Lib.Agent.AcceptHarness
    ( AcceptEnv (..)
    , withAcceptEnv
    , sampleTestRun
    , seedConfigAndPending
    , acceptViaAgent
    , foldPendingRequests
    , tokenRequestRefIds
    , readPendingFacts
    , readRunningFacts
    , factTestRun
    )
where

import Cli (Command (AgentCommand, GetFacts), cmd)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, finally)
import Core.Context (withContext)
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , Duration (..)
    , FaultsEnabled (..)
    , GithubRepository (..)
    , GithubUsername (..)
    , HasInstrumentation (..)
    , Owner
    , Platform (..)
    , RequestRefId
    , TokenId
    , Try (..)
    )
import Core.Types.Fact (Fact (..))
import Core.Types.MPFS (MPFSClient (..), newClient)
import Core.Types.Mnemonics (Mnemonics (ClearText))
import Core.Types.Tx
    ( TxHash
    , WithTxHash (..)
    )
import Core.Types.Wallet (Wallet (..))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive (mk)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Text (Text)
import Effects (mkEffects)
import Facts
    ( All (..)
    , FactsSelection (..)
    , TestRunSelection (..)
    )
import GitHub (Auth (..))
import MPFS.API
    ( MPFS (..)
    , RequestInsertBody (..)
    , awaitTransactionV2
    , mpfsClient
    )
import Oracle.Config.Types
    ( ConfigKey (..)
    , mkCurrentConfig
    )
import Oracle.Token.Cli
    ( TokenCommand (..)
    , TokenUpdateFailure
    , tokenCmdCore
    )
import Oracle.Types
    ( Token (..)
    , requestZooRefId
    )
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Requests.TestRun.Update (UpdateTestRunFailure)
import Oracle.Validate.Types (AValidationResult (..))
import Servant.Client (ClientM)
import Submitting (IfToWait (..), Submission (..), readWallet)
import System.Environment (getEnv, lookupEnv)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue
    , ToJSON (..)
    , renderCanonicalJSON
    )
import User.Agent.Cli (AgentCommand (Accept))
import User.Agent.Types (TestRunId)
import User.Types
    ( Phase (..)
    , TestRun (..)
    , TestRunState (..)
    )

-- | A booted token plus the wallets and credentials the Accept cycle
-- needs.
data AcceptEnv = AcceptEnv
    { aeClient :: MPFSClient
    , aeAuth :: Auth
    , aeOracle :: Wallet
    , aeAgent :: Wallet
    , aeTokenId :: TokenId
    }

-- | Read the devnet host + role wallets from the environment, boot a
-- fresh token (owned by the oracle), run the action, then end the token.
withAcceptEnv :: (AcceptEnv -> IO a) -> IO a
withAcceptEnv action = do
    host <- getEnv "MOOG_MPFS_HOST"
    client <- newClient (host, Wait 180, 120)
    oracle <- loadWallet "MOOG_TEST_ORACLE_WALLET"
    agent <- loadWallet "MOOG_TEST_AGENT_WALLET"
    auth <- loadAuth
    tokenId <- bootToken client auth oracle
    let env = AcceptEnv client auth oracle agent tokenId
    action env `finally` endTokenQuietly env

-- | A fixed test-run used by the Accept spec. Its content is never
-- validated by the Accept path, so concrete (offline) values suffice.
sampleTestRun :: TestRun
sampleTestRun =
    TestRun
        (Platform "linux")
        (GithubRepository (mk "cardano-foundation") (mk "moog"))
        (Directory ".")
        (Commit "0000000000000000000000000000000000000000")
        (Try 1)
        (GithubUsername (mk "moog-itest-requester"))

-- | Seed the two facts an agent Accept depends on — the token 'Config'
-- (with @configAgent@ = the agent owner) and the Pending test-run fact —
-- by inserting them and folding them into the token directly (oracle
-- signed), bypassing moog request validation.
seedConfigAndPending :: AcceptEnv -> TestRun -> IO ()
seedConfigAndPending env testRun = do
    let config =
            mkCurrentConfig
                (owner (aeAgent env))
                (TestRunValidationConfig{maxDuration = 7200, minDuration = 0})
        pending = mkPendingState (aeOracle env) testRun
    insertRequest env (jsonOf ConfigKey) (jsonOf config)
    insertRequest env (jsonOf testRun) (jsonOf pending)
    refIds <- tokenRequestRefIds env
    foldDirect env refIds

-- | Drive the real agent Accept (Pending→Running) for the test-run,
-- signing with the agent wallet — exactly what the agent process'
-- @submitRunning@ does.
acceptViaAgent
    :: AcceptEnv
    -> TestRunId
    -> IO
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState 'RunningT))
        )
acceptViaAgent AcceptEnv{aeClient, aeAuth, aeAgent, aeTokenId} trId = do
    res <-
        cmd
            $ AgentCommand aeAuth aeClient
            $ Accept aeTokenId aeAgent trId ()
    case res of
        ValidationSuccess (WithTxHash txh _) -> waitConfirmed aeClient txh
        _ -> pure ()
    pure res

-- | Fold the given requests into the token via the real oracle
-- @UpdateToken@ path (which re-validates each request). Used to
-- materialize the agent's Accept request as a Running fact.
foldPendingRequests
    :: AcceptEnv
    -> [RequestRefId]
    -> IO (AValidationResult TokenUpdateFailure TxHash)
foldPendingRequests AcceptEnv{aeClient, aeAuth, aeOracle, aeTokenId} refIds = do
    let MPFSClient{runMPFS, submitTx} = aeClient
    res <-
        runMPFS
            $ withContext mpfsClient (mkEffects aeAuth) submitTx
            $ tokenCmdCore
            $ UpdateToken aeTokenId aeOracle refIds
    case res of
        ValidationSuccess txh -> waitConfirmed aeClient txh
        _ -> pure ()
    pure res

-- | The outstanding request ref ids on the token.
tokenRequestRefIds :: AcceptEnv -> IO [RequestRefId]
tokenRequestRefIds AcceptEnv{aeClient, aeTokenId} = do
    let MPFSClient{runMPFS} = aeClient
    j <- runMPFS $ mpfsGetToken mpfsClient aeTokenId
    case fromJSON j of
        Just token ->
            pure $ requestZooRefId . runIdentity <$> tokenRequests token
        Nothing -> error "tokenRequestRefIds: could not parse token"

-- | Pending test-run facts on the token (the agent's pending poll
-- input).
readPendingFacts
    :: AcceptEnv -> IO [Fact TestRun (TestRunState 'PendingT)]
readPendingFacts AcceptEnv{aeClient, aeTokenId} =
    cmd $ GetFacts aeClient aeTokenId (TestRunFacts (TestRunPending [] All))

-- | Running (accepted) test-run facts on the token.
readRunningFacts
    :: AcceptEnv -> IO [Fact TestRun (TestRunState 'RunningT)]
readRunningFacts AcceptEnv{aeClient, aeTokenId} =
    cmd $ GetFacts aeClient aeTokenId (TestRunFacts (TestRunRunning [] All))

factTestRun :: Fact TestRun v -> TestRun
factTestRun (Fact k _ _) = k

-- Internal helpers --------------------------------------------------------

jsonOf :: ToJSON Identity a => a -> JSValue
jsonOf = runIdentity . toJSON

bootToken :: MPFSClient -> Auth -> Wallet -> IO TokenId
bootToken client@MPFSClient{runMPFS, submitTx} auth oracle = do
    res <-
        runMPFS
            $ withContext mpfsClient (mkEffects auth) submitTx
            $ tokenCmdCore
            $ BootToken oracle
    case res of
        ValidationSuccess (WithTxHash txh (Just tokenId)) -> do
            waitConfirmed client txh
            pure tokenId
        ValidationSuccess (WithTxHash _ Nothing) ->
            error "bootToken: no TokenId returned"
        ValidationFailure err ->
            error $ "bootToken failed: " <> show err

endTokenQuietly :: AcceptEnv -> IO ()
endTokenQuietly AcceptEnv{aeClient, aeAuth, aeOracle, aeTokenId} =
    ( do
        let MPFSClient{runMPFS, submitTx} = aeClient
        txh <-
            runMPFS
                $ withContext mpfsClient (mkEffects aeAuth) submitTx
                $ tokenCmdCore
                $ EndToken aeTokenId aeOracle
        waitConfirmed aeClient txh
    )
        `catch` \(_ :: SomeException) -> pure ()

insertRequest :: AcceptEnv -> JSValue -> JSValue -> IO ()
insertRequest AcceptEnv{aeClient, aeOracle, aeTokenId} key value = do
    let MPFSClient{runMPFS, submitTx} = aeClient
        Submission submit = submitTx aeOracle
    WithTxHash txh _ <-
        runMPFS
            $ submit
            $ \address ->
                mpfsRequestInsertFromFacts
                    mpfsClient
                    address
                    aeTokenId
                    RequestInsertBody{key, value}
    waitConfirmed aeClient txh

foldDirect :: AcceptEnv -> [RequestRefId] -> IO ()
foldDirect AcceptEnv{aeClient, aeOracle, aeTokenId} refIds = do
    let MPFSClient{runMPFS, submitTx} = aeClient
        Submission submit = submitTx aeOracle
    WithTxHash txh _ <-
        runMPFS
            $ submit
            $ \address ->
                mpfsUpdateTokenFromFacts mpfsClient address aeTokenId refIds
    waitConfirmed aeClient txh

-- | A Pending state with a real (but, for Accept, unverified) Ed25519
-- signature over the test-run by the given wallet's key.
mkPendingState :: Wallet -> TestRun -> TestRunState 'PendingT
mkPendingState wallet testRun =
    Pending
        (Duration 3600)
        (FaultsEnabled True)
        (HasInstrumentation True)
        signature
  where
    sk = privateKey wallet
    pk = Ed25519.toPublic sk
    message = BL.toStrict $ renderCanonicalJSON $ jsonOf testRun
    signature = Ed25519.sign sk pk message

waitConfirmed :: MPFSClient -> TxHash -> IO ()
waitConfirmed MPFSClient{runMPFS} txh = go (300 :: Int)
  where
    go 0 = error "waitConfirmed: transaction not confirmed in time"
    go n =
        runMPFS (awaitTransactionV2 txh)
            `catch` \(_ :: SomeException) -> do
                threadDelay 1_000_000
                go (n - 1)

loadAuth :: IO Auth
loadAuth =
    maybe (OAuth "unused") (OAuth . BC.pack) <$> lookupEnv "MOOG_GITHUB_PAT"

newtype UnencryptedWallet = UnencryptedWallet Text

instance Aeson.FromJSON UnencryptedWallet where
    parseJSON =
        Aeson.withObject "UnencryptedWallet" $ \v ->
            UnencryptedWallet <$> v Aeson..: "mnemonics"

loadWallet :: String -> IO Wallet
loadWallet envVar = do
    mfile <- lookupEnv envVar
    file <-
        maybe (error $ envVar <> " is not set") pure mfile
    content <- BC.readFile file
    case Aeson.decodeStrict content of
        Just (UnencryptedWallet mnemonics) ->
            case readWallet (True, ClearText mnemonics) of
                Right wallet -> pure wallet
                Left err ->
                    error $ "loadWallet " <> file <> ": " <> show err
        Nothing -> error $ "loadWallet: cannot decode " <> file
