{-# LANGUAGE DuplicateRecordFields #-}

module Oracle.Validate.Requests.RegisterUserSpec
    ( spec
    , genForRole
    )
where

import Control.Monad (when)
import Core.Types.Basic
    ( Owner (..)
    , Platform (..)
    , PublicKeyHash (..)
    , RequestRefId (..)
    , Username (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (toJSFact)
import Core.Types.Operation (Op (OpD, OpI), Operation (..))
import Data.Char (isAscii)
import Lib.SSH.Public
    ( SSHPublicKey (..)
    , encodeSSHPublicKey
    , extractPublicKeyHash
    )
import MockMPFS (mockMPFS, withFacts, withRequests)
import Oracle.Types (Request (..), RequestZoo (..))
import Oracle.Validate.Requests.RegisterUser
    ( RegisterUserFailure (..)
    , UnregisterUserFailure (..)
    , validateRegisterUser
    , validateUnregisterUser
    )
import Oracle.Validate.Requests.TestRun.Lib
    ( MockValidation (..)
    , mkValidation
    , noValidation
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
import Test.QuickCheck.Crypton (sshGen)
import Test.QuickCheck.EGen (EGen, egenProperty, gen, genBlind)
import Test.QuickCheck.JSString (genAscii)
import Test.QuickCheck.Lib (withAPresence, withAPresenceInAList)
import User.Types (RegisterUserKey (..))
import Validation (KeyFailure (..))
import Validation.RegisterUser (PublicKeyFailure (..))

genUserDBElement :: Gen (Username, SSHPublicKey)
genUserDBElement = do
    user <- Username <$> genAscii
    pk <- SSHPublicKey <$> genAscii
    pure (user, pk)

genValidDBElement :: EGen (Username, SSHPublicKey)
genValidDBElement = do
    user <- gen $ Username <$> genAscii
    (_sign, pk) <- genBlind sshGen
    pure (user, encodeSSHPublicKey pk)

registerUserChange
    :: Platform
    -> Username
    -> PublicKeyHash
    -> Change RegisterUserKey (OpI ())
registerUserChange platform username pubkeyhash =
    Change
        { key =
            Key
                $ RegisterUserKey
                    { platform
                    , username
                    , pubkeyhash
                    }
        , operation = Insert ()
        }

unregisterUserChange
    :: Platform
    -> Username
    -> PublicKeyHash
    -> Change RegisterUserKey (OpD ())
unregisterUserChange platform username pubkeyhash =
    Change
        { key =
            Key
                $ RegisterUserKey
                    { platform
                    , username
                    , pubkeyhash
                    }
        , operation = Delete ()
        }

newtype OtherSSHPublicKey = OtherSSHPublicKey String
    deriving (Show, Eq)

genForRole :: EGen ForRole
genForRole = gen $ oneof [pure ForOracle, pure ForUser]

genDBElementOther :: Gen (Username, OtherSSHPublicKey)
genDBElementOther = do
    user <- Username <$> arbitrary `suchThat` all isAscii
    pk <- arbitrary `suchThat` all isAscii
    pure (user, OtherSSHPublicKey $ "ssh-rsa " <> pk)

spec :: Spec
spec = do
    describe "validate requester requests" $ do
        it "validate a registered user" $ egenProperty $ do
            e@(user, pk) <- genValidDBElement
            forRole <- genForRole
            let validation =
                    mkValidation mockMPFS
                        $ noValidation
                            { mockUserKeys = [e]
                            }
                test =
                    validateRegisterUser validation forRole
                        $ registerUserChange (Platform "github") user
                        $ extractPublicKeyHash pk
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated
        it "fail to validate a registration if the user is already registered"
            $ egenProperty
            $ do
                e@(user, pk) <- genValidDBElement
                forRole <- genForRole
                let platform = "github"
                    pubkey@(PublicKeyHash stringPk) = extractPublicKeyHash pk
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey
                            }
                fact <- toJSFact registration ()
                (_, pk1) <- genValidDBElement
                let pubkey1 = extractPublicKeyHash pk1
                let
                    validation =
                        mkValidation (withFacts [fact] mockMPFS)
                            $ noValidation{mockUserKeys = [e, (user, pk1)]}
                    test =
                        validateRegisterUser validation forRole
                            $ registerUserChange
                                (Platform platform)
                                user
                                pubkey1
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RegisterUserKeyAlreadyExists stringPk)
        it
            "fail to validate a registration if the request is already pending"
            $ egenProperty
            $ do
                (user, pk) <- genValidDBElement
                forRole <- genForRole
                let platform = "github"
                    pubkey = extractPublicKeyHash pk
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey
                            }
                    change = registerUserChange (Platform platform) user pubkey
                    otherChange = unregisterUserChange (Platform platform) user pubkey
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
                let validation =
                        mkValidation
                            (withRequests db mockMPFS)
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
                db <- gen $ withAPresenceInAList 0.5 e genUserDBElement
                platform <- gen $ withAPresence 0.5 "github" arbitrary
                let validation =
                        mkValidation mockMPFS
                            $ noValidation{mockUserKeys = db}
                    test =
                        validateRegisterUser validation forRole
                            $ registerUserChange (Platform platform) user
                            $ extractPublicKeyHash pk
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
                let platform = "github"
                    pubkey = extractPublicKeyHash pk
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey
                            }
                fact <- toJSFact registration ()
                let validation =
                        mkValidation (withFacts [fact] mockMPFS)
                            $ noValidation{mockUserKeys = [e]}
                    test =
                        validateRegisterUser validation forRole
                            $ registerUserChange (Platform platform) user pubkey
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
                let platform = "github"
                    pubkey = extractPublicKeyHash pk
                let validation = mkValidation mockMPFS noValidation
                    test =
                        validateRegisterUser validation forRole
                            $ registerUserChange (Platform platform) user pubkey
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (PublicKeyValidationFailure NoPublicKeyFound)

        it
            "fail to validate a registration if there is no ssh-ed25519 public key for a user"
            $ egenProperty
            $ do
                (user, OtherSSHPublicKey pk) <- gen genDBElementOther
                forRole <- genForRole
                let platform = "github"
                    extractOtherPublicKeyHash sshPk =
                        let
                            expectedPrefix = "ssh-rsa " :: String
                        in
                            PublicKeyHash $ drop (length expectedPrefix) sshPk
                    pubkey = extractOtherPublicKeyHash pk
                    e = (user, SSHPublicKey pk)
                let validation = mkValidation mockMPFS $ noValidation{mockUserKeys = [e]}
                    test =
                        validateRegisterUser validation forRole
                            $ registerUserChange (Platform platform) user pubkey
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (PublicKeyValidationFailure NoEd25519KeyFound)

        it
            "fail to validate a registration if there is different ssh-ed25519 public key for a user"
            $ egenProperty
            $ do
                e@(user, pk1) <- genValidDBElement
                forRole <- genForRole
                (_, pk2) <- genValidDBElement
                let platform = "github"
                    pubkey = extractPublicKeyHash pk2
                let validation = mkValidation mockMPFS $ noValidation{mockUserKeys = [e]}
                    test =
                        validateRegisterUser validation forRole
                            $ registerUserChange (Platform platform) user pubkey
                pure
                    $ when (pk1 /= pk2)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (PublicKeyValidationFailure NoEd25519KeyMatch)

        it
            "validate an unregistration if there is a given user already registered"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                forRole <- genForRole
                let platform = "github"
                    pubkey = extractPublicKeyHash pk
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey
                            }
                fact <- toJSFact registration ()
                let validation =
                        mkValidation
                            (withFacts [fact] mockMPFS)
                            noValidation
                    test =
                        validateUnregisterUser validation forRole
                            $ unregisterUserChange (Platform platform) user pubkey
                pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it
            "fail to validate an unregistration if the request is already pending"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                forRole <- genForRole
                let platform = "github"
                    pubkey = extractPublicKeyHash pk
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey
                            }
                    change = unregisterUserChange (Platform platform) user pubkey
                    requestAnimal =
                        RegisterUserRequest
                            $ Request
                                { outputRefId = RequestRefId "animal"
                                , owner = Owner ""
                                , change = registerUserChange (Platform platform) user pubkey
                                }
                db <- genBlind $ oneof [pure [], pure [requestAnimal]]
                let validation =
                        mkValidation (withRequests db mockMPFS) noValidation
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
                    pubkey = extractPublicKeyHash pk
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey
                            }
                fact <- toJSFact registration ()
                let validation =
                        mkValidation
                            (withFacts [fact] mockMPFS)
                            noValidation
                    test =
                        validateUnregisterUser validation forRole
                            $ unregisterUserChange (Platform platform) userOther pubkey
                    registrationOther =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = userOther
                            , pubkeyhash = pubkey
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
                            , pubkeyhash = pubkey
                            }
                    pubkey = extractPublicKeyHash pk
                fact <- toJSFact registration ()
                let validation = mkValidation (withFacts [fact] mockMPFS) noValidation
                    test =
                        validateUnregisterUser validation forRole
                            $ unregisterUserChange (Platform platform) user pubkey
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
                    pubkey = extractPublicKeyHash pk
                    pubkey1 = extractPublicKeyHash pk1
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey
                            }
                fact <- toJSFact registration ()
                let validation =
                        mkValidation
                            (withFacts [fact] mockMPFS)
                            noValidation
                    test =
                        validateUnregisterUser validation forRole
                            $ unregisterUserChange (Platform platform) user pubkey1
                    registrationOther =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey1
                            }
                pure
                    $ when (pubkey /= pubkey1)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UnregisterUserKeyFailure (KeyDoesNotExist $ show registrationOther))
