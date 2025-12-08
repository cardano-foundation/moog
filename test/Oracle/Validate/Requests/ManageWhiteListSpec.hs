{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Oracle.Validate.Requests.ManageWhiteListSpec (spec)
where

import Control.Monad (unless, when)
import Core.Types.Basic
    ( GithubRepository (..)
    , Owner (..)
    , Platform (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (toJSFact)
import Core.Types.Operation (Op (OpD, OpI), Operation (..))
import Effects (KeyFailure (..))
import MockMPFS (mockMPFS, withFacts)
import Oracle.Validate.Requests.ManageWhiteList
    ( UpdateWhiteListFailure (..)
    , validateAddWhiteListed
    , validateRemoveWhiteListed
    )
import Oracle.Validate.Requests.TestRun.Lib
    ( MockValidation (..)
    , mkEffects
    , noValidation
    , testConfigFactGen
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
    ( oneof
    )
import Test.QuickCheck.EGen (egenProperty, gen, genBlind)
import Test.QuickCheck.JSString (genAscii)
import Test.QuickCheck.Lib (withAPresence, withAPresenceInAList)
import User.Agent.Types (WhiteListKey (..))
import User.Agent.TypesSpec (genRepository)

addWhiteListKey
    :: Platform
    -> GithubRepository
    -> Change WhiteListKey (OpI ())
addWhiteListKey platform repo =
    Change
        { key = Key $ WhiteListKey platform repo
        , operation = Insert ()
        }

removeWhiteListKey
    :: Platform
    -> GithubRepository
    -> Change WhiteListKey (OpD ())
removeWhiteListKey platform repo =
    Change
        { key = Key $ WhiteListKey platform repo
        , operation = Delete ()
        }

spec :: Spec
spec = do
    describe "validation of whitelist management" $ do
        it "validates a repository insertion" $ egenProperty $ do
            repo <- gen genRepository
            agent <- Owner <$> gen genAscii
            configFact <- testConfigFactGen agent
            let validation =
                    mkEffects (withFacts [configFact] mockMPFS)
                        $ noValidation
                            { mockReposExists = [repo]
                            }
                test =
                    validateAddWhiteListed
                        validation
                        agent
                        agent
                        $ addWhiteListKey (Platform "github") repo
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it
            "fails to validate an insertion for a platform different from GitHub"
            $ egenProperty
            $ do
                repo <- gen genRepository
                agent <- Owner <$> gen genAscii
                platform <- gen $ withAPresence 0.5 "github" genAscii
                configFact <- testConfigFactGen agent
                let validation =
                        mkEffects (withFacts [configFact] mockMPFS)
                            $ noValidation
                                { mockReposExists = [repo]
                                }
                    test =
                        validateAddWhiteListed
                            validation
                            agent
                            agent
                            $ addWhiteListKey (Platform platform) repo
                pure
                    $ when (platform /= "github")
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (WhiteListPlatformUnsupported $ Platform platform)

        it
            "fails to validate a repository insertion if the repository is not in github"
            $ egenProperty
            $ do
                repo <- gen genRepository
                agent <- Owner <$> gen genAscii
                presence <- gen $ withAPresenceInAList 0.5 repo genRepository
                configFact <- testConfigFactGen agent
                let validation =
                        mkEffects (withFacts [configFact] mockMPFS)
                            $ noValidation
                                { mockReposExists = presence
                                }
                    test =
                        validateAddWhiteListed
                            validation
                            agent
                            agent
                            $ addWhiteListKey (Platform "github") repo
                pure
                    $ unless (repo `elem` presence)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (WhiteListRepositoryNotInThePlatform repo)
        it
            "fails to validate a repository insertion if the repository is already whitelisted"
            $ egenProperty
            $ do
                repo <- gen genRepository
                agent <- Owner <$> gen genAscii
                presenceInGithub <-
                    gen
                        $ withAPresenceInAList
                            0.5
                            repo
                            genRepository
                let platform = Platform "github"
                    insertion = addWhiteListKey platform repo
                    key = WhiteListKey platform repo
                fact <- toJSFact key () 0
                configFact <- testConfigFactGen agent
                presenceInFacts <-
                    gen $ oneof [pure [], pure [fact]]
                let validation =
                        mkEffects (withFacts (configFact : presenceInFacts) mockMPFS)
                            $ noValidation
                                { mockReposExists = presenceInGithub
                                }
                    test =
                        validateAddWhiteListed
                            validation
                            agent
                            agent
                            insertion
                pure
                    $ when (fact `elem` presenceInFacts)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        ( WhiteListRepositoryKeyValidation
                            (KeyAlreadyExists $ show key)
                        )

        it "validates a repository removal" $ egenProperty $ do
            repo <- gen genRepository
            agent <- Owner <$> gen genAscii
            let platform = Platform "github"
                removal = removeWhiteListKey platform repo
                key = WhiteListKey platform repo
            fact <- toJSFact key () 0
            configFact <- testConfigFactGen agent
            presenceInGithub <-
                gen $ withAPresenceInAList 0.5 repo genRepository
            let validation =
                    mkEffects (withFacts [configFact, fact] mockMPFS)
                        $ noValidation
                            { mockReposExists = presenceInGithub
                            }
                test =
                    validateRemoveWhiteListed
                        validation
                        agent
                        agent
                        removal
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it
            "fail to validate a repository removal if the repository is not whitelisted"
            $ egenProperty
            $ do
                repo <- gen genRepository
                agent <- Owner <$> gen genAscii
                presenceInGithub <-
                    gen $ withAPresenceInAList 0.5 repo genRepository
                let platform = Platform "github"
                    removal = removeWhiteListKey platform repo
                    key = WhiteListKey platform repo
                fact <- toJSFact key () 0
                configFact <- testConfigFactGen agent
                presenceInFacts <-
                    gen $ oneof [pure [], pure [fact]]
                let validation =
                        mkEffects (withFacts (configFact : presenceInFacts) mockMPFS)
                            $ noValidation
                                { mockReposExists = presenceInGithub
                                }
                    test =
                        validateRemoveWhiteListed
                            validation
                            agent
                            agent
                            removal
                pure
                    $ unless (fact `elem` presenceInFacts)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        ( WhiteListRepositoryKeyValidation
                            (KeyDoesNotExist $ show key)
                        )
        it
            "fail to validate a repository removal if the platform is not GitHub"
            $ egenProperty
            $ do
                repo <- gen genRepository
                agent <- Owner <$> gen genAscii
                platform <- gen $ withAPresence 0.5 "github" genAscii
                let removal = removeWhiteListKey (Platform platform) repo
                    key = WhiteListKey (Platform platform) repo
                fact <- toJSFact key () 0
                configFact <- testConfigFactGen agent
                presenceInGithub <- gen $ withAPresenceInAList 0.5 repo genRepository
                let validation =
                        mkEffects (withFacts [configFact, fact] mockMPFS)
                            $ noValidation
                                { mockReposExists = presenceInGithub
                                }
                    test =
                        validateRemoveWhiteListed
                            validation
                            agent
                            agent
                            removal
                pure
                    $ when (platform /= "github")
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (WhiteListPlatformUnsupported $ Platform platform)
        it
            "fail to validate a whitelist operation if the agent is not the submitter"
            $ egenProperty
            $ do
                repo <- gen genRepository
                let genOwner = Owner <$> genAscii
                agent <- gen genOwner
                otherAgent <- gen $ withAPresence 0.5 agent genOwner
                platform <- fmap Platform $ gen $ withAPresence 0.5 "github" genAscii
                let
                    insertion = addWhiteListKey platform repo
                    removal = removeWhiteListKey platform repo
                    key = WhiteListKey platform repo
                fact <- toJSFact key () 0
                configFact <- testConfigFactGen agent
                presenceInGithub <-
                    gen $ withAPresenceInAList 0.5 repo genRepository
                presenceInFacts <-
                    gen $ oneof [pure [], pure [fact]]
                let validation =
                        mkEffects (withFacts (configFact : presenceInFacts) mockMPFS)
                            $ noValidation
                                { mockReposExists = presenceInGithub
                                }
                operation <-
                    genBlind
                        $ oneof
                            [ pure
                                $ validateAddWhiteListed
                                    validation
                                    agent
                                    otherAgent
                                    insertion
                            , pure
                                $ validateRemoveWhiteListed
                                    validation
                                    agent
                                    otherAgent
                                    removal
                            ]
                pure
                    $ when (agent /= otherAgent)
                    $ runValidate operation
                    `shouldReturn` ValidationFailure
                        (WhiteListAgentNotRecognized otherAgent)
