{-# LANGUAGE DuplicateRecordFields #-}

module Oracle.Validate.Requests.TestRun.AcceptSpec (spec)
where

import Control.Monad (when)
import Core.Types.Basic
    ( FaultsEnabled (..)
    , Owner (..)
    , RequestRefId (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Duration (Duration (Hours))
import Core.Types.Fact (toJSFact)
import Core.Types.Operation (Operation (..))
import MockMPFS (mockMPFS, withFacts, withRequests)
import Oracle.Types (Request (..), RequestZoo (AcceptRequest))
import Oracle.Validate.Requests.RegisterUserSpec (genForRole)
import Oracle.Validate.Requests.TestRun.Lib
    ( mkEffects
    , noValidation
    , signatureGen
    , testConfigFactGen
    , testRunEGen
    )
import Oracle.Validate.Requests.TestRun.Update
    ( AgentRejection (..)
    , UpdateTestRunFailure (..)
    , validateToRunningCore
    , validateToRunningUpdate
    )
import Oracle.Validate.Types
    ( AValidationResult (ValidationFailure)
    , forUser
    , runValidate
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldReturn
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Testable (..)
    , counterexample
    , oneof
    )
import Test.QuickCheck.EGen (egenProperty, gen, genA, genBlind)
import Test.QuickCheck.JSString (genAscii)
import Test.QuickCheck.Property (cover)
import User.Types
    ( TestRunState (..)
    )

spec :: Spec
spec = do
    describe "validate accept test-run requests" $ do
        it "validate an accept test run" $ egenProperty $ do
            testRun <- testRunEGen
            signature <- gen signatureGen
            faultsEnabled <- FaultsEnabled <$> gen arbitrary
            let pendingState = Pending (Hours 5) faultsEnabled signature
            testRunFact <- toJSFact testRun pendingState 0
            let validation =
                    mkEffects (withFacts [testRunFact] mockMPFS) noValidation
                newTestRunState = Accepted pendingState
                test = validateToRunningCore validation testRun newTestRunState
            pure $ test `shouldReturn` Nothing
        it
            "fail to validate an accept test-run if a test-run key is already pending"
            $ egenProperty
            $ do
                testRun <- testRunEGen
                signature <- gen signatureGen
                forRole <- genForRole
                anOwner <- gen $ Owner <$> genAscii
                faultsEnabled <- FaultsEnabled <$> gen arbitrary
                configFact <- testConfigFactGen anOwner
                let pendingState = Pending (Hours 5) faultsEnabled signature
                    change = Change (Key testRun) (Update pendingState (Accepted pendingState))
                    pendingRequest =
                        AcceptRequest
                            (Request{outputRefId = RequestRefId "", owner = anOwner, change})
                db <- genBlind $ oneof [pure [], pure [pendingRequest]]
                let validation =
                        mkEffects
                            (withRequests db (withFacts [configFact] mockMPFS))
                            noValidation
                    test = validateToRunningUpdate validation forRole anOwner anOwner change
                pure
                    $ when (not (null db) && forUser forRole)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UpdateTestRunKeyAlreadyPending testRun)
        it "fail to validate an accept for a non-existing test run"
            $ egenProperty
            $ do
                testRun <- testRunEGen
                signature <- gen signatureGen
                duration <- genA
                faultsEnabled <- FaultsEnabled <$> gen arbitrary
                let pendingState = Pending (Hours duration) faultsEnabled signature
                    newTestRunState = Accepted pendingState
                    test =
                        validateToRunningCore
                            (mkEffects mockMPFS noValidation)
                            testRun
                            newTestRunState
                pure $ test `shouldReturn` Just PreviousStateWrong

        it
            "fail to validate an accept for a pending test run with different state"
            $ egenProperty
            $ do
                duration <- genA
                differentDuration <- gen $ oneof [arbitrary, pure duration]
                testRun <- testRunEGen
                signature <- gen signatureGen
                differentSignature <- gen $ oneof [signatureGen, pure signature]
                faultsEnabled <- FaultsEnabled <$> gen arbitrary
                let fact = Pending (Hours duration) faultsEnabled signature
                    request =
                        Pending (Hours differentDuration) faultsEnabled differentSignature
                testRunFact <- toJSFact testRun fact 0
                let validation =
                        mkEffects (withFacts [testRunFact] mockMPFS) noValidation
                    newTestRunState = Accepted request
                    test = validateToRunningCore validation testRun newTestRunState
                pure
                    $ counterexample (show (fact, request))
                    $ cover 0.2 (fact == request) "enough success"
                    $ cover 0.7 (fact /= request) "enough failure"
                    $ property
                    $ do
                        if fact == request
                            then test `shouldReturn` Nothing
                            else test `shouldReturn` Just PreviousStateWrong
