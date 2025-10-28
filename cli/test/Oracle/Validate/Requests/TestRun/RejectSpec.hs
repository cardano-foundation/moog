{-# LANGUAGE DuplicateRecordFields #-}

module Oracle.Validate.Requests.TestRun.RejectSpec (spec)
where

import Control.Monad (when)
import Core.Types.Basic
    ( Duration (..)
    , Owner (..)
    , RequestRefId (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (toJSFact)
import Core.Types.Operation (Operation (..))
import MockMPFS (mockMPFS, withFacts, withRequests)
import Oracle.Types (Request (..), RequestZoo (..))
import Oracle.Validate.Requests.RegisterUserSpec (genForRole)
import Oracle.Validate.Requests.TestRun.Lib
    ( mkEffects
    , noValidation
    , signatureGen
    , testRunEGen
    )
import Oracle.Validate.Requests.TestRun.Update
    ( AgentRejection (..)
    , UpdateTestRunFailure (..)
    , validateToDoneCore
    , validateToDoneUpdate
    )
import Oracle.Validate.Types
    ( AValidationResult (..)
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
    ( TestRunRejection (BrokenInstructions)
    , TestRunState (..)
    )

spec :: Spec
spec = do
    describe "validate agent requests" $ do
        it "validate a reject test run" $ egenProperty $ do
            testRun <- testRunEGen
            signature <- gen signatureGen
            let pendingState = Pending (Duration 5) signature
            testRunFact <- toJSFact testRun pendingState 0
            let validation =
                    mkEffects
                        (withFacts [testRunFact] mockMPFS)
                        noValidation
                newTestRunState = Rejected pendingState [BrokenInstructions]
                test = validateToDoneCore validation testRun newTestRunState
            pure $ test `shouldReturn` Nothing
        it
            "fail to validate a rejected test-run request if a test-run key is already pending"
            $ egenProperty
            $ do
                testRun <- testRunEGen
                signature <- gen signatureGen
                forRole <- genForRole
                anOwner <- gen $ Owner <$> genAscii
                let pendingState = Pending (Duration 5) signature
                    change =
                        Change
                            { key = Key testRun
                            , operation =
                                Update
                                    pendingState
                                    (Rejected pendingState [])
                            }
                    pendingRequest =
                        RejectRequest
                            Request{outputRefId = RequestRefId "", owner = anOwner, change}
                db <- genBlind $ oneof [pure [], pure [pendingRequest]]
                let validation =
                        mkEffects
                            (withRequests db mockMPFS)
                            noValidation
                    test = validateToDoneUpdate validation forRole anOwner anOwner change
                pure
                    $ when (not (null db) && forUser forRole)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UpdateTestRunKeyAlreadyPending testRun)
        it "fail to validate a reject for a non-existing test run"
            $ egenProperty
            $ do
                testRun <- testRunEGen
                signature <- gen signatureGen
                duration <- genA
                let pendingState = Pending (Duration duration) signature
                    newTestRunState = Rejected pendingState [BrokenInstructions]
                    test =
                        validateToDoneCore
                            (mkEffects mockMPFS noValidation)
                            testRun
                            newTestRunState
                pure $ test `shouldReturn` Just PreviousStateWrong

        it
            "fail to validate a reject for a pending test run with different state"
            $ egenProperty
            $ do
                duration <- genA
                differentDuration <- gen $ oneof [arbitrary, pure duration]
                testRun <- testRunEGen
                signature <- gen signatureGen
                differentSignature <- gen $ oneof [signatureGen, pure signature]
                let fact = Pending (Duration duration) signature
                    request =
                        Pending (Duration differentDuration) differentSignature
                testRunFact <- toJSFact testRun fact 0
                let validation =
                        mkEffects
                            (withFacts [testRunFact] mockMPFS)
                            noValidation
                    newTestRunState = Rejected request [BrokenInstructions]
                    test = validateToDoneCore validation testRun newTestRunState
                pure
                    $ counterexample (show (fact, request))
                    $ cover 0.2 (fact == request) "enough success"
                    $ cover 0.7 (fact /= request) "enough failure"
                    $ property
                    $ do
                        if fact == request
                            then test `shouldReturn` Nothing
                            else test `shouldReturn` Just PreviousStateWrong
