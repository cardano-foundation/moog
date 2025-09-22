{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Oracle.Validate.Requests.TestRun.CreateSpec (spec)
where

import Control.Lens ((%~), (.~))
import Control.Monad (when)
import Core.Types.Basic
    ( Duration (..)
    , FileName (..)
    , Owner (..)
    , RequestRefId (RequestRefId)
    , organization
    , project
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (JSFact, toJSFact)
import Core.Types.Operation (Operation (..))
import Lib.SSH.Public (encodePublicKey)
import MockMPFS (mockMPFS, withFacts, withRequests)
import Oracle.Types (Request (..), RequestZoo (..))
import Oracle.Validate.DownloadAssets
    ( AssetValidationFailure (..)
    , SourceDirFailure (..)
    )
import Oracle.Validate.Requests.RegisterUserSpec (genForRole)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Requests.TestRun.Create
    ( CreateTestRunFailure (..)
    , TestRunRejection (..)
    , validateCreateTestRun
    , validateCreateTestRunCore
    )
import Oracle.Validate.Requests.TestRun.Lib
    ( MockValidation (..)
    , changeDirectory
    , changeOrganization
    , changePlatform
    , changeProject
    , changeRequester
    , changeTry
    , gitAsset
    , gitCommit
    , gitDirectory
    , jsFactRole
    , jsFactUser
    , mkValidation
    , noValidation
    , signTestRun
    , signatureGen
    , testConfigEGen
    , testRunEGen
    )
import Oracle.Validate.Types
    ( AValidationResult (ValidationFailure, ValidationSuccess)
    , Validated (..)
    , ValidationResult
    , forUser
    , runValidate
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldContain
    , shouldNotContain
    , shouldReturn
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Positive (..)
    , Testable (..)
    , counterexample
    , cover
    , elements
    , listOf
    , oneof
    , suchThat
    )
import Test.QuickCheck.Crypton (sshGen)
import Test.QuickCheck.EGen
    ( egenProperty
    , gen
    , genA
    , genBlind
    , genShrinkA
    )
import User.Agent.Types (WhiteListKey (..))
import User.Types
    ( RegisterRoleKey (..)
    , TestRun (..)
    , TestRunRejection (..)
    , TestRunState (..)
    , URL (..)
    , tryIndexL
    )

shouldHaveReason
    :: (Show a, Eq a) => ValidationResult [a] -> a -> IO ()
shouldHaveReason (ValidationSuccess _) _ = pure ()
shouldHaveReason (ValidationFailure reasons) reason =
    reasons `shouldContain` [reason]

shouldNotHaveReason
    :: (Show a, Eq a) => ValidationResult [a] -> a -> IO ()
shouldNotHaveReason (ValidationSuccess _) _ = pure ()
shouldNotHaveReason (ValidationFailure reasons) reason =
    reasons `shouldNotContain` [reason]

onConditionHaveReason
    :: (Show a, Eq a) => ValidationResult [a] -> a -> Bool -> IO ()
onConditionHaveReason result reason = \case
    True -> shouldHaveReason result reason
    False -> shouldNotHaveReason result reason

spec :: Spec
spec = do
    describe "validate create-test request" $ do
        it "accepts valid test run" $ egenProperty $ do
            testRun <- testRunEGen
            testConfig <- testConfigEGen
            Positive duration <-
                gen
                    $ arbitrary
                        `suchThat` \(Positive d) ->
                            d >= testConfig.minDuration
                                && d <= testConfig.maxDuration
            (sign, pk) <- genBlind sshGen
            user <- jsFactUser testRun $ encodePublicKey pk
            role <- jsFactRole testRun
            whiteListRepo <-
                toJSFact
                    WhiteListKey
                        { platform = testRun.platform
                        , repository = testRun.repository
                        }
                    ()
            let previous = case tryIndex testRun of
                    1 -> []
                    n -> do
                        let previousTestRun = testRun{tryIndex = n - 1}
                        previousState <-
                            Pending (Duration duration)
                                <$> signTestRun
                                    sign
                                    previousTestRun
                        toJSFact previousTestRun previousState
                facts = [user, role, whiteListRepo] <> previous
                commit = gitCommit testRun
                directory = gitDirectory testRun
                files =
                    gitAsset testRun (FileName "README.md") "Test file"
                        <> gitAsset
                            testRun
                            (FileName "docker-compose.yaml")
                            "version: '3'"
                        <> gitAsset
                            testRun
                            (FileName "testnet.yaml")
                            "testnet: true"

                validation =
                    mkValidation (withFacts facts mockMPFS)
                        $ noValidation
                            { mockCommits = [commit]
                            , mockDirectories = [directory]
                            , mockAssets = files
                            }
            testRunState <-
                Pending (Duration duration)
                    <$> signTestRun sign testRun
            pure $ do
                mresult <-
                    runValidate
                        $ validateCreateTestRunCore
                            testConfig
                            validation
                            testRun
                            testRunState
                mresult `shouldBe` ValidationSuccess Validated
        it
            "fail to validate a create-test-run if the test-run key is already pending"
            $ egenProperty
            $ do
                testRun <- testRunEGen
                testConfig <- testConfigEGen
                Positive duration <-
                    gen
                        $ arbitrary
                            `suchThat` \(Positive d) ->
                                d >= testConfig.minDuration
                                    && d <= testConfig.maxDuration
                Positive otherDuration <-
                    gen
                        $ arbitrary
                            `suchThat` \(Positive d) ->
                                d >= testConfig.minDuration
                                    && d <= testConfig.maxDuration
                (sign, _pk) <- genBlind sshGen
                forRole <- genForRole
                testRunState <-
                    Pending (Duration duration)
                        <$> signTestRun sign testRun
                otherTestRunState <-
                    Pending (Duration otherDuration)
                        <$> signTestRun sign testRun
                let pendingRequest =
                        CreateTestRequest
                            Request
                                { outputRefId = RequestRefId ""
                                , owner = Owner ""
                                , change =
                                    Change
                                        { key = Key testRun
                                        , operation = Insert otherTestRunState
                                        }
                                }
                db <- genBlind $ oneof [pure [], pure [pendingRequest]]
                let validation =
                        mkValidation (withRequests db mockMPFS) noValidation
                pure
                    $ when (not (null db) && forUser forRole)
                    $ do
                        runValidate
                            $ validateCreateTestRun
                                testConfig
                                validation
                                forRole
                                Change
                                    { key = Key testRun
                                    , operation = Insert testRunState
                                    }
                    `shouldReturn` ValidationFailure
                        (CreateTestRunKeyAlreadyPending testRun)
        it "reports unacceptable duration" $ egenProperty $ do
            duration <- genShrinkA
            testRun <- testRunEGen
            testConfig <- testConfigEGen
            signature <- gen signatureGen
            let testRunState = Pending (Duration duration) signature
            pure $ do
                mresult <-
                    runValidate
                        $ validateCreateTestRunCore
                            testConfig
                            (mkValidation mockMPFS noValidation)
                            testRun
                            testRunState
                let expectedMinDuration = minDuration testConfig
                let expectedMaxDuration = maxDuration testConfig
                onConditionHaveReason
                    mresult
                    (UnacceptableDuration expectedMinDuration expectedMaxDuration)
                    $ duration < expectedMinDuration
                        || duration > expectedMaxDuration

        it "reports unacceptable role" $ egenProperty $ do
            testConfig <- testConfigEGen
            duration <- genA
            signature <- gen signatureGen
            testRunRequest <- testRunEGen
            testRunFact <-
                gen
                    $ oneof
                        [ changePlatform testRunRequest
                        , changeRequester testRunRequest
                        , changeOrganization testRunRequest
                        , changeProject testRunRequest
                        , pure testRunRequest
                        ]
            roleFact <- jsFactRole testRunFact
            let validation =
                    mkValidation
                        (withFacts [roleFact] mockMPFS)
                        noValidation
                testRunState = Pending (Duration duration) signature
            pure $ do
                mresult <-
                    runValidate
                        $ validateCreateTestRunCore
                            testConfig
                            validation
                            testRunRequest
                            testRunState
                let role =
                        RegisterRoleKey
                            testRunRequest.platform
                            testRunRequest.repository
                            testRunRequest.requester
                onConditionHaveReason mresult (UnacceptableRole role)
                    $ testRunRequest.platform /= testRunFact.platform
                        || testRunRequest.repository.organization
                            /= testRunFact.repository.organization
                        || testRunRequest.repository.project
                            /= testRunFact.repository.project
                        || testRunRequest.requester /= testRunFact.requester

        it "reports unacceptable try index" $ egenProperty $ do
            testConfig <- testConfigEGen
            duration <- genA
            signature <- gen signatureGen
            testRunR <- testRunEGen
            testRunDB <-
                gen
                    $ oneof
                        [ pure $ tryIndexL .~ 0 $ testRunR
                        , pure testRunR
                        ]
            testRun <-
                gen
                    $ oneof
                        [ changeTry testRunDB
                        , pure $ tryIndexL %~ succ $ testRunDB
                        ]
            let mkTestRunFact :: Monad m => TestRunState x -> m JSFact
                mkTestRunFact = toJSFact testRunDB
                pending = Pending (Duration duration) signature
                accepted = Accepted pending
            rejections <-
                gen $ listOf $ elements [BrokenInstructions, UnclearIntent]
            let rejected = Rejected pending rejections
            finalDuration <- genA
            finalURL <- genA
            let finished =
                    Finished accepted (Duration finalDuration) (URL finalURL)
            testRunStateDB <-
                gen
                    $ oneof
                        [ mkTestRunFact pending
                        , mkTestRunFact accepted
                        , mkTestRunFact rejected
                        , mkTestRunFact finished
                        ]
            testRunFact <- toJSFact testRunDB testRunStateDB
            let validation =
                    mkValidation
                        (withFacts [testRunFact | testRunDB.tryIndex > 0] mockMPFS)
                        noValidation
            let testRunState = Pending (Duration duration) signature
            pure
                $ counterexample (show testRunDB)
                $ cover
                    0.1
                    (testRunDB.tryIndex > 0)
                    "enough stored facts"
                $ cover
                    0.1
                    (testRun.tryIndex == testRunDB.tryIndex + 1)
                    "enough success"
                $ property
                $ do
                    mresult <-
                        runValidate
                            $ validateCreateTestRunCore
                                testConfig
                                validation
                                testRun
                                testRunState
                    let maxTry = testRunDB.tryIndex
                    onConditionHaveReason mresult (UnacceptableTryIndex maxTry)
                        $ testRun.tryIndex /= testRunDB.tryIndex + 1

        it "reports unacceptable directory" $ egenProperty $ do
            testConfig <- testConfigEGen
            duration <- genA
            signature <- gen signatureGen
            testRun <- testRunEGen
            testRun' <- gen $ oneof [changeDirectory testRun, pure testRun]
            let testRunState = Pending (Duration duration) signature
            testRunFact <- toJSFact testRun' testRunState
            let validation =
                    mkValidation (withFacts [testRunFact] mockMPFS)
                        $ noValidation
                            { mockDirectories = [gitDirectory testRun']
                            }
            pure $ do
                mresult <-
                    runValidate
                        $ validateCreateTestRunCore
                            testConfig
                            validation
                            testRun
                            testRunState
                onConditionHaveReason
                    mresult
                    ( UnacceptableAssets
                        (AssetValidationSourceFailure SourceDirFailureDirAbsent)
                    )
                    $ testRun /= testRun'
        it "reports not whitelisted repository" $ egenProperty $ do
            testConfig <- testConfigEGen
            duration <- genA
            signature <- gen signatureGen
            testRun <- testRunEGen
            let testRunState = Pending (Duration duration) signature
                key =
                    WhiteListKey
                        { platform = testRun.platform
                        , repository = testRun.repository
                        }
            whiteListFact <- toJSFact key ()
            whiteListed <-
                gen
                    $ oneof
                        [pure [], pure [whiteListFact]]
            let validation =
                    mkValidation (withFacts whiteListed mockMPFS) noValidation
            pure $ do
                mresult <-
                    runValidate
                        $ validateCreateTestRunCore
                            testConfig
                            validation
                            testRun
                            testRunState
                onConditionHaveReason mresult RepositoryNotWhitelisted
                    $ notElem whiteListFact (whiteListed :: [JSFact])
