module Pop.CliSpec where

import Pop.Cli (Result (..), Runtime(..), pop)
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec (Spec, it, shouldReturn, shouldThrow)
import Network.HTTP.Simple (Response, getResponseStatus)
import Network.HTTP.Types.Status (status200)
import qualified Data.ByteString.Lazy as BL

-- | A fake runtime that always returns a 200 OK response
fakeRuntime :: Runtime
fakeRuntime = Runtime { httpCall = \_ -> pure $ undefined }

spec :: Spec
spec = do
    it "can display --help" $ do
        let args = ["--help"]

        pop fakeRuntime args `shouldThrow` \e -> e == ExitSuccess

    it "can request user registration" $ do
        let args =
                [ "register"
                , "--platform"
                , "github"
                , "--username"
                , "bob"
                , "--pubkeyhash"
                , "607a0d8a64616a407537edf0d9b59cf4cb509c556f6d2de4250ce15df2"
                ]

        pop fakeRuntime args `shouldReturn` RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}

    it "can request adding user to a project" $ do
        let args =
                [ "add-user"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--role"
                , "maintainer"
                , "--user-id"
                , "github/bob"
                ]

        pop fakeRuntime args `shouldReturn` RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}

    it "can request antithesis run" $ do
        let args =
                [ "request"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--commit"
                , "9114528e2343e6fcf3c92de71364275227e6b16d"
b                ]

        pop fakeRuntime args `shouldReturn` RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}

    it "can request removing user from a project" $ do
        let args =
                [ "remove-user"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--user-id"
                , "github/bob"
                ]

        pop fakeRuntime args `shouldReturn` RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}
