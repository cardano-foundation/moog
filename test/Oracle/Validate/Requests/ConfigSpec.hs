{-# LANGUAGE DuplicateRecordFields #-}

module Oracle.Validate.Requests.ConfigSpec (spec)
where

import Control.Monad (when)
import Core.Types.Basic (Owner (..))
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation (Operation (..))
import MockMPFS (mockMPFS)
import Oracle.Config.Types (Config (..), ConfigKey (..))
import Oracle.Validate.Requests.Config
    ( ConfigFailure (..)
    , validateInsertConfig
    )
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Requests.TestRun.Lib
    ( mkEffects
    , noValidation
    )
import Oracle.Validate.Types
    ( AValidationResult (..)
    , Validated (..)
    , runValidate
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldReturn
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , suchThat
    )
import Test.QuickCheck.EGen (egenProperty, gen)
import Test.QuickCheck.JSString (genAscii)
import Test.QuickCheck.Lib (withAPresence)

spec :: Spec
spec = do
    describe "validation of config management" $ do
        it "validates a config insertion" $ egenProperty $ do
            minDuration <- gen $ arbitrary `suchThat` (> 0)
            maxDuration <- gen $ arbitrary `suchThat` (> minDuration)
            oracleOwner <- gen $ Owner <$> genAscii
            let
                agentOwner = Owner "agent"
                change =
                    Change (Key ConfigKey)
                        $ Insert
                        $ Config
                            { configAgent = agentOwner
                            , configTestRun =
                                TestRunValidationConfig
                                    { minDuration
                                    , maxDuration
                                    }
                            }
            let test =
                    validateInsertConfig
                        (mkEffects mockMPFS noValidation)
                        oracleOwner
                        oracleOwner
                        change
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated
        it
            "fails to validate a config insertion with a minimum duration less than 1"
            $ egenProperty
            $ do
                minDuration <- gen $ arbitrary `suchThat` (<= 0)
                maxDuration <- gen $ arbitrary `suchThat` (> minDuration)
                oracleOwner <- gen $ Owner <$> genAscii
                let
                    agentOwner = Owner "agent"
                    change =
                        Change (Key ConfigKey)
                            $ Insert
                            $ Config
                                { configAgent = agentOwner
                                , configTestRun =
                                    TestRunValidationConfig
                                        { minDuration
                                        , maxDuration
                                        }
                                }
                let test =
                        validateInsertConfig
                            (mkEffects mockMPFS noValidation)
                            oracleOwner
                            oracleOwner
                            change
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (ConfigureMinLessThanOne minDuration)
        it
            "fails to validate a config insertion with a maximum duration less than the minimum"
            $ egenProperty
            $ do
                minDuration <- gen $ arbitrary `suchThat` (> 0)
                maxDuration <- gen $ arbitrary `suchThat` (< minDuration)
                oracleOwner <- gen $ Owner <$> genAscii
                let
                    agentOwner = Owner "agent"
                    change =
                        Change (Key ConfigKey)
                            $ Insert
                            $ Config
                                { configAgent = agentOwner
                                , configTestRun =
                                    TestRunValidationConfig
                                        { minDuration
                                        , maxDuration
                                        }
                                }
                let test =
                        validateInsertConfig
                            (mkEffects mockMPFS noValidation)
                            oracleOwner
                            oracleOwner
                            change
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (ConfigureMaxLessThanMin maxDuration minDuration)
        it
            "fails to validate a config insertion if the submitter is not the oracle"
            $ egenProperty
            $ do
                minDuration <- gen $ arbitrary `suchThat` (> 0)
                maxDuration <- gen $ arbitrary `suchThat` (> minDuration)
                oracleOwner <- gen $ Owner <$> genAscii
                attacker <-
                    gen
                        $ withAPresence
                            0.5
                            oracleOwner
                            (Owner <$> arbitrary)
                let
                    change =
                        Change (Key ConfigKey)
                            $ Insert
                            $ Config
                                { configAgent = Owner "agent"
                                , configTestRun =
                                    TestRunValidationConfig
                                        { minDuration
                                        , maxDuration
                                        }
                                }
                let test =
                        validateInsertConfig
                            (mkEffects mockMPFS noValidation)
                            oracleOwner
                            attacker
                            change
                pure
                    $ when (attacker /= oracleOwner)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (ConfigureNotFromOracle attacker)
