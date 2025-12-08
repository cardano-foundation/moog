{-# LANGUAGE DuplicateRecordFields #-}

module Oracle.Validate.Requests.RegisterUserSpec
    ( spec
    , genForRole
    , unsafeEncodeVKey
    )
where

import Control.Monad (when)
import Core.Types.Basic
    ( GithubUsername (..)
    , Owner (..)
    , Platform (..)
    , RequestRefId (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (toJSFact)
import Core.Types.Operation (Op (OpD, OpI), Operation (..))
import Core.Types.VKey (encodeVKey)
import Crypto.PubKey.Ed25519 (PublicKey)
import Data.CaseInsensitive (mk)
import Data.Char (isAscii)
import Effects (KeyFailure (..))
import Effects.RegisterUser
    ( VKey (..)
    , VKeyFailure (..)
    )
import MockMPFS (mockMPFS, withFacts, withRequests)
import Oracle.Types (Request (..), RequestZoo (..))
import Oracle.TypesSpec (genGithubIdentification)
import Oracle.Validate.Requests.RegisterUser
    ( RegisterUserFailure (..)
    , UnregisterUserFailure (..)
    , validateRegisterUser
    , validateUnregisterUser
    )
import Oracle.Validate.Requests.TestRun.Lib
    ( MockValidation (..)
    , mkEffects
    , noValidation
    , testConfigFactGen
    )
import Oracle.Validate.Types
    ( AValidationResult (..)
    , ForRole (..)
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
import Test.QuickCheck.Crypton (ed25519Gen)
import Test.QuickCheck.EGen (EGen, egenProperty, gen, genBlind)
import Test.QuickCheck.JSString (genAscii)
import Test.QuickCheck.Lib (withAPresence, withAPresenceInAList)
import User.Types (GithubIdentification (..), RegisterUserKey (..))

genUserDBElement :: Gen (GithubUsername, GithubIdentification)
genUserDBElement = do
    user <- GithubUsername . mk <$> genAscii
    pk <- genGithubIdentification
    pure (user, pk)

unsafeEncodeVKey :: PublicKey -> VKey
unsafeEncodeVKey pk =
    case encodeVKey pk of
        Left err -> error $ "Failed to encode VKey: " ++ show err
        Right vkey -> vkey

genValidDBElement :: EGen (GithubUsername, GithubIdentification)
genValidDBElement = do
    user <- gen $ GithubUsername . mk <$> genAscii
    (_sign, pk) <- genBlind ed25519Gen
    fmap (user,)
        $ gen
        $ pure
        $ IdentifyViaVKey
        $ unsafeEncodeVKey pk

registerUserChange
    :: Platform
    -> GithubUsername
    -> GithubIdentification
    -> Change RegisterUserKey (OpI ())
registerUserChange platform username githubIdentification =
    Change
        { key =
            Key
                $ RegisterUserKey
                    { platform
                    , username
                    , githubIdentification
                    }
        , operation = Insert ()
        }

unregisterUserChange
    :: Platform
    -> GithubUsername
    -> GithubIdentification
    -> Change RegisterUserKey (OpD ())
unregisterUserChange platform username githubIdentification =
    Change
        { key =
            Key
                $ RegisterUserKey
                    { platform
                    , username
                    , githubIdentification
                    }
        , operation = Delete ()
        }

newtype OtherGithubIdentification
    = OtherGithubIdentification GithubIdentification
    deriving (Show, Eq)

genForRole :: EGen ForRole
genForRole = gen $ oneof [pure ForOracle, pure ForUser]

spec :: Spec
spec = do
    describe "validate requester requests" $ do
        it "validate a registered user" $ egenProperty $ do
            e@(user, pk) <- genValidDBElement
            forRole <- genForRole
            anOwner <- gen $ Owner <$> genAscii
            configFact <- testConfigFactGen anOwner
            let validation =
                    mkEffects (withFacts [configFact] mockMPFS)
                        $ noValidation
                            { mockIdentifications = [e]
                            }
                test =
                    validateRegisterUser validation forRole
                        $ registerUserChange (Platform "github") user pk
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated
        it "fail to validate a registration if the user is already registered"
            $ egenProperty
            $ do
                e@(user, pk) <- genValidDBElement
                forRole <- genForRole
                let platform = "github"
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , githubIdentification = pk
                            }
                fact <- toJSFact registration () 0
                (_, pk1) <- genValidDBElement
                anOwner <- gen $ Owner <$> genAscii
                configFact <- testConfigFactGen anOwner
                let
                    validation =
                        mkEffects (withFacts [configFact, fact] mockMPFS)
                            $ noValidation{mockIdentifications = [e, (user, pk1)]}
                    test =
                        validateRegisterUser validation forRole
                            $ registerUserChange
                                (Platform platform)
                                user
                                pk1
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RegisterUserKeyAlreadyExists pk)
        it
            "fail to validate a registration if the request is already pending"
            $ egenProperty
            $ do
                (user, pk) <- genValidDBElement
                forRole <- genForRole
                let platform = "github"
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , githubIdentification = pk
                            }
                    change = registerUserChange (Platform platform) user pk
                    otherChange = unregisterUserChange (Platform platform) user pk
                    pendingRequest b c =
                        b
                            $ Request
                                { outputRefId = RequestRefId "animal"
                                , owner = Owner "owner"
                                , change = c
                                }
                db <-
                    genBlind
                        $ oneof
                            [ pure []
                            , pure [pendingRequest RegisterUserRequest change]
                            , pure [pendingRequest UnregisterUserRequest otherChange]
                            , pure
                                [ pendingRequest RegisterUserRequest change
                                , pendingRequest UnregisterUserRequest otherChange
                                ]
                            ]
                anOwner <- gen $ Owner <$> genAscii
                configFact <- testConfigFactGen anOwner
                let validation =
                        mkEffects
                            (withFacts [configFact] (withRequests db mockMPFS))
                            noValidation
                    test =
                        validateRegisterUser validation forRole change
                pure
                    $ when (not (null db) && forUser forRole)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RegisterUserKeyChangeAlreadyPending registration)
        it
            "fail to validate a registration of user for an unsupported platform"
            $ egenProperty
            $ do
                e@(user, pk) <- gen genUserDBElement
                forRole <- genForRole
                anOwner <- gen $ Owner <$> genAscii
                configFact <- testConfigFactGen anOwner
                db <- gen $ withAPresenceInAList 0.5 e genUserDBElement
                platform <- gen $ withAPresence 0.5 "github" arbitrary
                let validation =
                        mkEffects (withFacts [configFact] mockMPFS)
                            $ noValidation{mockIdentifications = db}
                    test =
                        validateRegisterUser validation forRole
                            $ registerUserChange (Platform platform) user pk
                pure
                    $ when (platform /= "github")
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RegisterUserPlatformNotSupported platform)

        it
            "fail to validate a registration if a user already registered within a given valid platform"
            $ egenProperty
            $ do
                e@(user, pk) <- gen genUserDBElement
                forRole <- genForRole
                anOwner <- gen $ Owner <$> genAscii
                configFact <- testConfigFactGen anOwner
                let platform = "github"
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , githubIdentification = pk
                            }
                fact <- toJSFact registration () 0
                let validation =
                        mkEffects (withFacts [configFact, fact] mockMPFS)
                            $ noValidation{mockIdentifications = [e]}
                    test =
                        validateRegisterUser validation forRole
                            $ registerUserChange (Platform platform) user pk
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RegisterUserKeyFailure (KeyAlreadyExists $ show registration))

        it
            "fail to validate a registration if there is no public key for a user"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                forRole <- genForRole
                anOwner <- gen $ Owner <$> genAscii
                configFact <- testConfigFactGen anOwner
                let platform = "github"
                let validation = mkEffects (withFacts [configFact] mockMPFS) noValidation
                    test =
                        validateRegisterUser validation forRole
                            $ registerUserChange (Platform platform) user pk
                    failure = case pk of
                        IdentifyViaVKey _ -> VKeyValidationFailure VKeyNotFound
                        _ -> error "Expected IdentifyViaVKey"
                pure $ runValidate test `shouldReturn` ValidationFailure failure
        it
            "fail to validate a registration if there is different ssh-ed25519 public key for a user"
            $ egenProperty
            $ do
                e@(user, pk1) <- genValidDBElement
                forRole <- genForRole
                (_, pk2) <- genValidDBElement
                anOwner <- gen $ Owner <$> genAscii
                configFact <- testConfigFactGen anOwner
                let platform = "github"
                let validation =
                        mkEffects (withFacts [configFact] mockMPFS)
                            $ noValidation{mockIdentifications = [e]}
                    test =
                        validateRegisterUser validation forRole
                            $ registerUserChange (Platform platform) user pk2
                    failure = case (pk1, pk2) of
                        (IdentifyViaVKey k1, IdentifyViaVKey _k2) -> VKeyValidationFailure $ VKeyMismatch k1
                        (IdentifyViaSSHKey _, IdentifyViaVKey _) -> VKeyValidationFailure VKeyNotFound
                        _ -> error "Expected IdentifyViaVKey"
                pure
                    $ when (pk1 /= pk2)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        failure
        it
            "validate an unregistration if there is a given user already registered"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                forRole <- genForRole
                anOwner <- gen $ Owner <$> genAscii
                configFact <- testConfigFactGen anOwner
                let platform = "github"
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , githubIdentification = pk
                            }
                fact <- toJSFact registration () 0
                let validation =
                        mkEffects
                            (withFacts [configFact, fact] mockMPFS)
                            noValidation
                    test =
                        validateUnregisterUser validation forRole
                            $ unregisterUserChange (Platform platform) user pk
                pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it
            "fail to validate an unregistration if the request is already pending"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                forRole <- genForRole
                let platform = "github"
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , githubIdentification = pk
                            }
                    change = unregisterUserChange (Platform platform) user pk
                    requestAnimal =
                        RegisterUserRequest
                            $ Request
                                { outputRefId = RequestRefId "animal"
                                , owner = Owner ""
                                , change = registerUserChange (Platform platform) user pk
                                }
                db <- genBlind $ oneof [pure [], pure [requestAnimal]]
                anOwner <- gen $ Owner <$> genAscii
                configFact <- testConfigFactGen anOwner
                let validation =
                        mkEffects
                            (withRequests db (withFacts [configFact] mockMPFS))
                            noValidation
                    test =
                        validateUnregisterUser validation forRole change
                pure
                    $ when (not (null db) && forUser forRole)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UnregisterUserKeyChangeAlreadyPending registration)

        it
            "fail to validate an unregistration if there is no a given user already registered"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                (userOther, _) <- gen genUserDBElement
                forRole <- genForRole
                let platform = "github"
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , githubIdentification = pk
                            }
                fact <- toJSFact registration () 0
                anOwner <- gen $ Owner <$> genAscii
                configFact <- testConfigFactGen anOwner
                let validation =
                        mkEffects
                            (withFacts [configFact, fact] mockMPFS)
                            noValidation
                    test =
                        validateUnregisterUser validation forRole
                            $ unregisterUserChange (Platform platform) userOther pk
                    registrationOther =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = userOther
                            , githubIdentification = pk
                            }
                pure
                    $ when (user /= userOther)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UnregisterUserKeyFailure (KeyDoesNotExist $ show registrationOther))

        it
            "fail to validate an unregistration of user for an unsupported platform"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                forRole <- genForRole
                platform <-
                    gen $ withAPresence 0.5 "github" arbitrary `suchThat` all isAscii
                let registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , githubIdentification = pk
                            }
                fact <- toJSFact registration () 0
                anOwner <- gen $ Owner <$> genAscii
                configFact <- testConfigFactGen anOwner
                let validation = mkEffects (withFacts [configFact, fact] mockMPFS) noValidation
                    test =
                        validateUnregisterUser validation forRole
                            $ unregisterUserChange (Platform platform) user pk
                pure
                    $ when (platform /= "github")
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UnregisterUserPlatformNotSupported platform)

        it
            "fail to validate an unregistration if there is a given user has different public key"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                (_, pk1) <- gen genUserDBElement
                forRole <- genForRole
                let platform = "github"
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , githubIdentification = pk
                            }
                fact <- toJSFact registration () 0
                anOwner <- gen $ Owner <$> genAscii
                configFact <- testConfigFactGen anOwner
                let validation =
                        mkEffects
                            (withFacts [fact, configFact] mockMPFS)
                            noValidation
                    test =
                        validateUnregisterUser validation forRole
                            $ unregisterUserChange (Platform platform) user pk1
                    registrationOther =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , githubIdentification = pk1
                            }
                pure
                    $ when (pk /= pk1)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UnregisterUserKeyFailure (KeyDoesNotExist $ show registrationOther))
