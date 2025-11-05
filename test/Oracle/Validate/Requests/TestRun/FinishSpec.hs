{-# LANGUAGE DuplicateRecordFields #-}

module Oracle.Validate.Requests.TestRun.FinishSpec (spec)
where

import Control.Monad (when)
import Core.Types.Basic
    ( Duration (..)
    , FaultsEnabled (..)
    , Owner (..)
    , RequestRefId (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (toJSFact)
import Core.Types.Operation (Operation (..))
import MockMPFS (mockMPFS, withFacts, withRequests)
import Oracle.Types (Request (..), RequestZoo (FinishedRequest))
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
    , validateToRunningCore
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
    ( Outcome (OutcomeSuccess)
    , TestRunState (..)
    , URL (..)
    )

spec :: Spec
spec = do
    describe "validate finished test-run requests" $ do
        it "validate an accept test run" $ egenProperty $ do
            testRun <- testRunEGen
            signature <- gen signatureGen
            actualDuration <- genA
            url <- gen genAscii
            faultsEnabled <- FaultsEnabled <$> gen arbitrary
            let acceptedState = Accepted $ Pending (Duration 5) faultsEnabled signature
            testRunFact <- toJSFact testRun acceptedState 0
            let validation =
                    mkEffects
                        (withFacts [testRunFact] mockMPFS)
                        noValidation
                newTestRunState =
                    Finished
                        acceptedState
                        (Duration actualDuration)
                        OutcomeSuccess
                        (URL url)
                test = validateToDoneCore validation testRun newTestRunState
            pure $ test `shouldReturn` Nothing
        it
            "fail to validate a finished test-run request if a test-run key is already pending"
            $ egenProperty
            $ do
                testRun <- testRunEGen
                signature <- gen signatureGen
                forRole <- genForRole
                anOwner <- gen $ Owner <$> genAscii
                let pendingState = Accepted (Pending (Duration 5) (FaultsEnabled True) signature)
                    change =
                        Change
                            { key = Key testRun
                            , operation =
                                Update
                                    pendingState
                                    (Finished pendingState (Duration 1) OutcomeSuccess (URL ""))
                            }
                    pendingRequest =
                        FinishedRequest
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
        it "fail to validate an accept for a non-existing test run"
            $ egenProperty
            $ do
                testRun <- testRunEGen
                signature <- gen signatureGen
                duration <- genA
                let pendingState = Pending (Duration duration) (FaultsEnabled True) signature
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
                pendingDuration <- genA
                differentPendingDuration <-
                    gen $ oneof [arbitrary, pure pendingDuration]
                testRun <- testRunEGen
                signature <- gen signatureGen
                differentSignature <- gen $ oneof [signatureGen, pure signature]
                finishedDuration <- genA
                faultsEnabled <- FaultsEnabled <$> gen arbitrary
                url <- genA
                let fact =
                        Accepted $ Pending (Duration pendingDuration) faultsEnabled signature
                    request =
                        Accepted
                            $ Pending
                                (Duration differentPendingDuration)
                                faultsEnabled
                                differentSignature
                testRunFact <- toJSFact testRun fact 0
                let validation =
                        mkEffects
                            (withFacts [testRunFact] mockMPFS)
                            noValidation
                    newTestRunState =
                        Finished
                            request
                            (Duration finishedDuration)
                            OutcomeSuccess
                            (URL url)
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
