{-# LANGUAGE OverloadedStrings #-}

-- | Tests that the oracle config flow routes its on-chain writes through
-- the facts-based MPFS operations rather than the legacy endpoints.
module Oracle.Config.CliSpec
    ( spec
    ) where

import Core.Context (withContext)
import Core.Types.Basic
    ( Address (..)
    , Owner (..)
    , Success (..)
    , TokenId (..)
    )
import Core.Types.Fact (JSFact, toJSFact)
import Core.Types.Tx
    ( TxHash (..)
    , UnsignedTx (..)
    , WithTxHash (..)
    , WithUnsignedTx (..)
    )
import Core.Types.Wallet (Wallet)
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import MPFS.API (MPFS (..))
import MockMPFS (mockMPFS, withFacts)
import Oracle.Config.Cli (ConfigCmd (..), configCmd)
import Oracle.Config.Types
    ( Config
    , ConfigKey (..)
    , mkCurrentConfig
    )
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Requests.TestRun.Lib (mkEffects, noValidation)
import Submitting (Submission (..))
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Text.JSON.Canonical (JSValue)

spec :: Spec
spec =
    describe "Oracle.Config.Cli facts-based writes" $ do
        it "routes a config insert through mpfsRequestInsertFromFacts" $ do
            let result = runConfig markedMPFS
            -- empty config facts -> insert branch
            txHash result `shouldBe` TxHash "facts-insert"

        it "routes a config update through mpfsRequestUpdateFromFacts" $ do
            let result = runConfig (withFacts [configFact] markedMPFS)
            -- one existing config fact -> update branch
            txHash result `shouldBe` TxHash "facts-update"

-- | Run the @SetConfig@ command against a mock MPFS, returning the
-- submitted transaction wrapper. The submission surfaces which builder
-- ran via the unsigned-tx marker promoted into the tx hash.
runConfig :: MPFS Identity -> WithTxHash Success
runConfig mpfs =
    runIdentity
        $ withContext
            mpfs
            (\m _ -> mkEffects m noValidation)
            (const recordingSubmission)
            (configCmd (SetConfig sampleToken sampleWallet sampleConfig))

-- | A mock MPFS where each write op returns a distinct marker, so the
-- test can observe which one the flow selected.
markedMPFS :: MPFS Identity
markedMPFS =
    mockMPFS
        { mpfsRequestInsert = \_ _ _ -> pure (marker "legacy-insert")
        , mpfsRequestUpdate = \_ _ _ -> pure (marker "legacy-update")
        , mpfsRequestInsertFromFacts =
            \_ _ _ -> pure (marker "facts-insert")
        , mpfsRequestUpdateFromFacts =
            \_ _ _ -> pure (marker "facts-update")
        }

-- | Build an unsigned-tx wrapper whose transaction text is a marker
-- identifying the builder that produced it.
marker :: Text -> WithUnsignedTx JSValue
marker name = WithUnsignedTx (UnsignedTx name) Nothing

-- | A submission that promotes the unsigned-tx marker into the tx hash,
-- so a test can assert which MPFS builder was invoked.
recordingSubmission :: Submission Identity
recordingSubmission = Submission $ \action -> do
    WithUnsignedTx (UnsignedTx tx) value <- action (Address "addr")
    pure (WithTxHash (TxHash tx) value)

sampleToken :: TokenId
sampleToken = TokenId "deadbeef"

-- | The config flow never inspects the wallet (the mock submission
-- ignores it), so a bottom is safe and keeps the fixture light.
sampleWallet :: Wallet
sampleWallet = error "wallet is not forced by the config flow"

sampleConfig :: Config
sampleConfig =
    mkCurrentConfig
        (Owner "agent")
        TestRunValidationConfig{minDuration = 1, maxDuration = 2}

configFact :: JSFact
configFact = runIdentity $ toJSFact ConfigKey sampleConfig 0
