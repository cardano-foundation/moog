module Lib.Github.OracleValidationSpec
    ( userSpec
    , roleSpecs
    , existenceSpec
    , vkeySpec
    )
where

import Core.Types.Basic
    ( FileName (..)
    , GithubRepository (..)
    , GithubUsername (..)
    )
import Data.Text qualified as T
import Effects.RegisterRole
    ( RepositoryRoleFailure (..)
    , inspectRepoRoleForUserTemplate
    )
import Effects.RegisterUser
    ( SSHPublicKeyFailure (..)
    , VKey (..)
    , VKeyFailure (..)
    , inspectPublicKeyTemplate
    , inspectVKey
    )
import GitHub (Auth)
import Lib.GitHub
    ( CodeOwnersFailure (CodeOwnersFailure)
    , GetGithubFileFailure (..)
    , githubGetAntiCLIVKey
    , githubGetCodeOwnersFile
    , githubRepositoryExists
    )
import Lib.SSH.Public (makeSSHPublicKey)
import Test.Hspec
    ( Spec
    , SpecWith
    , describe
    , it
    , shouldReturn
    )

existenceSpec :: SpecWith Auth
existenceSpec = do
    describe "githubRepositoryExists" $ do
        it "should return true for hal-fixture-sin" $ \auth -> do
            githubRepositoryExists
                auth
                (GithubRepository "cardano-foundation" "hal-fixture-sin")
                `shouldReturn` Right True
        it "should return false for hal-fixture-son" $ \auth -> do
            githubRepositoryExists
                auth
                (GithubRepository "cardano-foundation" "hal-fixture-son")
                `shouldReturn` Right False

roleSpecs :: SpecWith Auth
roleSpecs = do
    it "should download CODEOWNERS file from repo with main" $ \auth -> do
        githubGetCodeOwnersFile
            auth
            (GithubRepository "cardano-foundation" "hal-fixture-sin")
            `shouldReturn` Right "antithesis: @notunrandom @cfhal\n"

    it "should download CODEOWNERS file from repo with master" $ \auth -> do
        githubGetCodeOwnersFile
            auth
            (GithubRepository "cardano-foundation" "hal-fixture-cos")
            `shouldReturn` Right "* @notunrandom\n"

    it "should download CODEOWNERS file from repo with trunk" $ \auth -> do
        githubGetCodeOwnersFile
            auth
            (GithubRepository "cardano-foundation" "hal-fixture-tan")
            `shouldReturn` Right "* @notunrandom\n"

    it "should throw if missing CODEOWNERS file" $ \auth -> do
        githubGetCodeOwnersFile
            auth
            (GithubRepository "cardano-foundation" "hal-fixture-sec")
            `shouldReturn` Left
                ( CodeOwnersFailure
                    [ (FileName "CODEOWNERS", GetGithubFileDirectoryNotFound)
                    , (FileName ".github/CODEOWNERS", GetGithubFileDirectoryNotFound)
                    , (FileName "docs/CODEOWNERS", GetGithubFileDirectoryNotFound)
                    ]
                )

vkeySpec :: SpecWith Auth
vkeySpec = do
    it "should download moog.vkey for cfhal" $ \auth -> do
        githubGetAntiCLIVKey
            auth
            (GithubUsername "cfhal")
            `shouldReturn` Right
                "vkey1lrqqrpr49593dv6jchcdlqpqj0y9rfpcaauscnhs74wc50z76aqsqqlrgh"
    it "should match moog.vkey for cfhal" $ \auth -> do
        inspectVKey
            auth
            (GithubUsername "cfhal")
            (VKey "vkey1lrqqrpr49593dv6jchcdlqpqj0y9rfpcaauscnhs74wc50z76aqsqqlrgh")
            `shouldReturn` Nothing
    it "should mismatch moog.vkey for cfhal" $ \auth -> do
        inspectVKey
            auth
            (GithubUsername "cfhal")
            (VKey "vkey1n5wwq8zr3ts65e37yus206r4dzau3sc5ant6mxt304atnfudutrqwdzd5")
            `shouldReturn` Just
                ( VKeyMismatch
                    ( VKey
                        "vkey1lrqqrpr49593dv6jchcdlqpqj0y9rfpcaauscnhs74wc50z76aqsqqlrgh"
                    )
                )

userSpec :: Spec
userSpec = do
    it "user needs to have public key(s) exposed"
        $ do
            let emptyPubKeyOfUser _ = pure $ Right []
                user = GithubUsername "user1"
                pubkey = makeSSHPublicKey ""
            inspectPublicKeyTemplate
                user
                pubkey
                emptyPubKeyOfUser
        `shouldReturn` Just NoPublicKeyFound

    it "user needs to have ssh-ed25519 public key exposed"
        $ do
            let respKey = "ssh-rsa AAAAAAAA"
                nonEd25519PubKeyOfUser _ = pure $ Right [respKey]
                user = GithubUsername "user1"
                pubkey = makeSSHPublicKey ""
            inspectPublicKeyTemplate
                user
                pubkey
                nonEd25519PubKeyOfUser
        `shouldReturn` Just NoEd25519KeyMatch

    it "user needs to the expected ssh-ed25519 public key exposed"
        $ do
            let respKey = "ssh-ed25519 AAAAAAAA"
                noExpectedEd25519PubKeyOfUser _ = pure $ Right [respKey]
                user = GithubUsername "user1"
                pubkey = makeSSHPublicKey "XAAAAAAY"
            inspectPublicKeyTemplate
                user
                pubkey
                noExpectedEd25519PubKeyOfUser
        `shouldReturn` Just NoEd25519KeyMatch

    it "user needs gets the expected ssh-ed25519 public key exposed 1"
        $ do
            let respKey = "ssh-ed25519 XAAAAAAY"
                okExpectedEd25519PubKeyOfUser _ = pure $ Right [respKey]
                user = GithubUsername "user1"
                pubkey = makeSSHPublicKey "XAAAAAAY"
            inspectPublicKeyTemplate
                user
                pubkey
                okExpectedEd25519PubKeyOfUser
        `shouldReturn` Nothing

    it "user needs gets the expected ssh-ed25519 public key exposed 1"
        $ do
            let respKey1 = "ssh-ed25519 XAAAAAAY"
                respKey2 = "ssh-ed25519 AAAAAAAA"
                respKey3 = "ssh-rsa XXXXXXXXXXXXXXXXXXXXXXx"
                okExpectedEd25519PubKeyOfUser _ = pure $ Right [respKey1, respKey2, respKey3]
                user = GithubUsername "user1"
                pubkey = makeSSHPublicKey "XAAAAAAY"
            inspectPublicKeyTemplate
                user
                pubkey
                okExpectedEd25519PubKeyOfUser
        `shouldReturn` Nothing

    it "CODEOWNERS does not have role entry" $ do
        let noRoleEntry _ =
                pure
                    $ Right
                    $ T.unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        ]
            user = GithubUsername "user1"
            repo = GithubRepository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` Just NoRoleEntryInCodeowners

    it "CODEOWNERS does not have users assigned" $ do
        let noRoleEntry _ =
                pure
                    $ Right
                    $ T.unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        , ""
                        , "antithesis:"
                        ]
            user = GithubUsername "user1"
            repo = GithubRepository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` Just NoUsersAssignedToRoleInCodeowners

    it "CODEOWNERS does have other users assigned" $ do
        let noRoleEntry _ =
                pure
                    $ Right
                    $ T.unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        , ""
                        , "antithesis: @user1 @user3"
                        ]
            user = GithubUsername "user2"
            repo = GithubRepository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` Just NoUserInCodeowners

    it "CODEOWNERS does have user assigned" $ do
        let noRoleEntry _ =
                pure
                    $ Right
                    $ T.unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        , ""
                        , "antithesis: @user1 @user2 @user3"
                        ]
            user = GithubUsername "user2"
            repo = GithubRepository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` Nothing
