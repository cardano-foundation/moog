{-# LANGUAGE DuplicateRecordFields #-}

module Oracle.Validate.Requests.RegisterRoleSpec (spec)
where

import Control.Monad (when)
import Core.Types.Basic
    ( GithubRepository (..)
    , GithubUsername (..)
    , Owner (..)
    , Platform (..)
    , RequestRefId (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (toJSFact)
import Core.Types.Operation (Op (OpD, OpI), Operation (..))
import Data.CaseInsensitive (mk)
import Data.Char (isAscii)
import Effects (KeyFailure (..))
import Effects.RegisterRole
    ( RepositoryRoleFailure (..)
    )
import MockMPFS (mockMPFS, withFacts, withRequests)
import Oracle.Types (Request (..), RequestZoo (..))
import Oracle.Validate.Requests.RegisterRole
    ( RegisterRoleFailure (..)
    , UnregisterRoleFailure (..)
    , validateRegisterRole
    , validateUnregisterRole
    )
import Oracle.Validate.Requests.RegisterUserSpec (genForRole)
import Oracle.Validate.Requests.TestRun.Lib
    ( MockValidation (..)
    , mkEffects
    , noValidation
    )
import Oracle.Validate.Types
    ( AValidationResult (..)
    , Validated (..)
    , forUser
    , runValidate
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldReturn
    )
import Test.QuickCheck (Gen, arbitrary, oneof, suchThat)
import Test.QuickCheck.EGen (egenProperty, gen, genBlind)
import Test.QuickCheck.Lib (withAPresence, withAPresenceInAList)
import User.Types (RegisterRoleKey (..))

genRoleDBElement :: Gen (GithubUsername, GithubRepository)
genRoleDBElement = do
    user <- GithubUsername . mk <$> arbitrary `suchThat` all isAscii
    repo <-
        GithubRepository . mk
            <$> arbitrary `suchThat` all isAscii
            <*> (mk <$> arbitrary `suchThat` all isAscii)
    pure (user, repo)

registerRoleChange
    :: Platform
    -> GithubUsername
    -> GithubRepository
    -> Change RegisterRoleKey (OpI ())
registerRoleChange platform user repo =
    Change
        { key =
            Key
                $ RegisterRoleKey
                    { platform = platform
                    , username = user
                    , repository = repo
                    }
        , operation = Insert ()
        }

unregisterRoleChange
    :: Platform
    -> GithubUsername
    -> GithubRepository
    -> Change RegisterRoleKey (OpD ())
unregisterRoleChange platform user repo =
    Change
        { key =
            Key
                $ RegisterRoleKey
                    { platform = platform
                    , username = user
                    , repository = repo
                    }
        , operation = Delete ()
        }

spec :: Spec
spec = do
    describe "validate requester requests" $ do
        it "validate a registered role" $ egenProperty $ do
            forRole <- genForRole
            e@(user, repo) <- gen genRoleDBElement
            let validation = mkEffects mockMPFS $ noValidation{mockRepoRoles = [e]}
                test =
                    validateRegisterRole validation forRole
                        $ registerRoleChange (Platform "github") user repo
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it
            "fail to validate a register role if a request for that key is already pending"
            $ egenProperty
            $ do
                (user, repo) <- gen genRoleDBElement
                forRole <- genForRole
                let platform = "github"
                    registration =
                        RegisterRoleKey
                            { platform = Platform platform
                            , username = user
                            , repository = repo
                            }
                    change =
                        Change
                            { key = Key registration
                            , operation = Insert ()
                            }
                    otherChange =
                        Change
                            { key = Key registration
                            , operation = Delete ()
                            }
                    pendingRequest b c =
                        b
                            $ Request
                                { outputRefId = RequestRefId "animal"
                                , owner = Owner ""
                                , change = c
                                }
                db <-
                    genBlind
                        $ oneof
                            [ pure []
                            , pure [pendingRequest RegisterRoleRequest change]
                            , pure [pendingRequest UnregisterRoleRequest otherChange]
                            , pure
                                [ pendingRequest RegisterRoleRequest change
                                , pendingRequest UnregisterRoleRequest otherChange
                                ]
                            ]
                let validation =
                        mkEffects
                            (withRequests db mockMPFS)
                            noValidation

                    test =
                        validateRegisterRole validation forRole
                            $ registerRoleChange (Platform platform) user repo
                pure
                    $ when (not (null db) && forUser forRole)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RegisterRoleKeyChangeAlreadyPending registration)
        it "fail to validate a role for an unsupported platform" $ egenProperty $ do
            e@(user, repo) <- gen genRoleDBElement
            forRole <- genForRole
            db <- gen $ withAPresenceInAList 0.5 e genRoleDBElement
            platform <- gen $ withAPresence 0.5 "github" arbitrary
            let validation = mkEffects mockMPFS $ noValidation{mockRepoRoles = db}
                test =
                    validateRegisterRole validation forRole
                        $ registerRoleChange (Platform platform) user repo
            pure
                $ when (platform /= "github")
                $ runValidate test
                `shouldReturn` ValidationFailure
                    (RegisterRolePlatformNotSupported platform)

        it
            "fail to validate a role registration if a user is already registered within a given valid platform"
            $ egenProperty
            $ do
                e@(user, repo) <- gen genRoleDBElement
                forRole <- genForRole
                let platform = "github"
                    registration =
                        RegisterRoleKey
                            { platform = Platform platform
                            , username = user
                            , repository = repo
                            }
                fact <- toJSFact registration () 0
                let validation =
                        mkEffects
                            (withFacts [fact] mockMPFS)
                            $ noValidation{mockRepoRoles = [e]}
                    test =
                        validateRegisterRole validation forRole
                            $ registerRoleChange (Platform platform) user repo
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RegisterRoleKeyFailure (KeyAlreadyExists $ show registration))

        it
            "fail to validate a role registration if there is no repo for a user present"
            $ egenProperty
            $ do
                (user, repo) <- gen genRoleDBElement
                forRole <- genForRole
                let platform = "github"
                    validation = mkEffects mockMPFS noValidation
                    test =
                        validateRegisterRole validation forRole
                            $ registerRoleChange (Platform platform) user repo
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RoleNotPresentOnPlatform NoRoleEntryInCodeowners)

        it
            "fail to validate a role registration if there is different repo-user pair 1"
            $ egenProperty
            $ do
                e@(user, repo) <- gen genRoleDBElement
                forRole <- genForRole
                (_, repo1) <- gen genRoleDBElement
                let platform = "github"
                    validation = mkEffects mockMPFS $ noValidation{mockRepoRoles = [e]}
                    test =
                        validateRegisterRole validation forRole
                            $ registerRoleChange (Platform platform) user repo1
                pure
                    $ when (repo /= repo1)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RoleNotPresentOnPlatform NoRoleEntryInCodeowners)

        it
            "fail to validate a role registration if there is different repo-user pair 1"
            $ egenProperty
            $ do
                e@(user, repo) <- gen genRoleDBElement
                (user1, repo1) <- gen genRoleDBElement
                forRole <- genForRole
                let platform = "github"
                    validation = mkEffects mockMPFS $ noValidation{mockRepoRoles = [e]}
                    test =
                        validateRegisterRole validation forRole
                            $ registerRoleChange (Platform platform) user1 repo1
                pure
                    $ when (repo /= repo1 && user /= user1)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RoleNotPresentOnPlatform NoRoleEntryInCodeowners)

        it
            "validate a role unregistration if there is a given user already registered"
            $ egenProperty
            $ do
                e@(user, repo) <- gen genRoleDBElement
                forRole <- genForRole
                let platform = "github"
                    registration =
                        RegisterRoleKey
                            { platform = Platform platform
                            , username = user
                            , repository = repo
                            }
                fact <- toJSFact registration () 0
                let validation =
                        mkEffects
                            (withFacts [fact] mockMPFS)
                            $ noValidation{mockRepoRoles = [e]}
                    test =
                        validateUnregisterRole validation forRole
                            $ unregisterRoleChange (Platform platform) user repo
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure UnregisterRoleRoleIsStillValidInGithub

        it
            "fail to validate a unregister role if a request for that key is already pending"
            $ egenProperty
            $ do
                (user, repo) <- gen genRoleDBElement
                forRole <- genForRole
                let platform = "github"
                    registration =
                        RegisterRoleKey
                            { platform = Platform platform
                            , username = user
                            , repository = repo
                            }
                    change =
                        Change
                            { key = Key registration
                            , operation = Delete ()
                            }
                    otherChange =
                        Change
                            { key = Key registration
                            , operation = Insert ()
                            }
                    pendingRequest b c =
                        b
                            $ Request
                                { outputRefId = RequestRefId "animal"
                                , owner = Owner ""
                                , change = c
                                }
                db <-
                    genBlind
                        $ oneof
                            [ pure []
                            , pure [pendingRequest UnregisterRoleRequest change]
                            , pure [pendingRequest RegisterRoleRequest otherChange]
                            , pure
                                [ pendingRequest UnregisterRoleRequest change
                                , pendingRequest RegisterRoleRequest otherChange
                                ]
                            ]
                let validation = mkEffects (withRequests db mockMPFS) noValidation
                    test =
                        validateUnregisterRole validation forRole
                            $ unregisterRoleChange (Platform platform) user repo
                pure
                    $ when (not (null db) && forUser forRole)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UnregisterRoleKeyChangeAlreadyPending registration)
        it
            "fail to validate a role unregistration if there is no a given user' role already registered"
            $ egenProperty
            $ do
                (user, repo) <- gen genRoleDBElement
                (userOther, _) <- gen genRoleDBElement
                forRole <- genForRole
                let platform = "github"
                    registration =
                        RegisterRoleKey
                            { platform = Platform platform
                            , username = user
                            , repository = repo
                            }
                fact <- toJSFact registration () 0
                let validation =
                        mkEffects
                            (withFacts [fact] mockMPFS)
                            noValidation
                    test =
                        validateUnregisterRole validation forRole
                            $ unregisterRoleChange (Platform platform) userOther repo
                    registrationOther =
                        RegisterRoleKey
                            { platform = Platform platform
                            , username = userOther
                            , repository = repo
                            }
                pure
                    $ when (user /= userOther)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UnregisterRoleKeyFailure (KeyDoesNotExist $ show registrationOther))
