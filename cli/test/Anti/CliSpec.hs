{-# LANGUAGE DuplicateRecordFields #-}
module Anti.CliSpec
    ( spec
    , runDummyServer
    , anti
    )
where

import Anti.Main (main)
import Anti.Server (appDummy, dummyTxId)
import Anti.Types
    ( Command (..)
    , Directory (..)
    , Host (..)
    , Options (..)
    , Platform (..)
    , Port (..)
    , PublicKeyHash (..)
    , Repository (..)
    , Role (..)
    , SHA1 (..)
    , TokenId (..)
    , UserCommand (..)
    , Username (..)
    , userCommand
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Data.Aeson (Value)
import Network.Wai.Handler.Warp (run)
import System.Environment (withArgs)
import Test.Hspec
    ( Spec
    , beforeAll_
    , it
    , shouldReturn
    )

runDummyServer :: IO ()
runDummyServer = do
    _ <- async $ do
        run 8084 appDummy
    threadDelay 1000000
    return ()

anti :: [String] -> IO (Options, Value)
anti args = do
    -- Simulate the command line arguments
    let args' =
            [ "--host"
            , "localhost"
            , "--port"
            , "8084"
            , "--token-id"
            , "dummyTokenId"
            ]
                ++ args
    -- Call the main function with the simulated arguments
    ev <- withArgs args' main
    case ev of
        (_, Left err) -> error $ "Error: " ++ show err
        (o, Right result) -> return (o, result)

spec :: Spec
spec = beforeAll_ runDummyServer $ do
    it "can request user registration" $ do
        let args =
                [ "register-public-key"
                , "--platform"
                , "github"
                , "--username"
                , "bob"
                , "--pubkeyhash"
                , "607a0d8a64616a407537edf0d9b59cf4cb509c556f6d2de4250ce15df2"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        UserCommand
                            { userCommand =
                                RegisterPublicKey
                                    { platform = Platform "github"
                                    , username = Username "bob"
                                    , pubkeyhash =
                                        PublicKeyHash
                                            "607a0d8a64616a407537edf0d9b59cf4cb509c556f6d2de4250ce15df2"
                                    }
                            , tokenId =
                                TokenId
                                    "dummyTokenId"
                            }
                    }
        anti args
            `shouldReturn` (opts, dummyTxId)
    it "can request user unregistration" $ do
        let args =
                [ "unregister-public-key"
                , "--platform"
                , "github"
                , "--username"
                , "bob"
                , "--pubkeyhash"
                , "607a0d8a64616a407537edf0d9b59cf4cb509c556f6d2de4250ce15df2"
                ]

        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        UserCommand
                            { userCommand =
                                UnregisterPublicKey
                                    { platform = Platform "github"
                                    , username = Username "bob"
                                    , pubkeyhash =
                                        PublicKeyHash
                                            "607a0d8a64616a407537edf0d9b59cf4cb509c556f6d2de4250ce15df2"
                                    }
                            , tokenId =
                                TokenId
                                    "dummyTokenId"
                            }
                    }
        anti args `shouldReturn` (opts, dummyTxId)

    it "can request adding user to a project" $ do
        let args =
                [ "register-role"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--role"
                , "maintainer"
                , "--username"
                , "bob"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        UserCommand
                            { userCommand =
                                RegisterRole
                                    { platform = Platform "github"
                                    , repository = Repository "cardano-foundation" "antithesis"
                                    , role = Role "maintainer"
                                    , username = Username "bob"
                                    }
                            , tokenId =
                                TokenId
                                    "dummyTokenId"
                            }
                    }

        anti args `shouldReturn` (opts, dummyTxId)

    it "can request removing user from a project" $ do
        let args =
                [ "unregister-role"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--role"
                , "maintainer"
                , "--username"
                , "bob"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        UserCommand
                            { userCommand =
                                UnregisterRole
                                    { platform = Platform "github"
                                    , repository = Repository "cardano-foundation" "antithesis"
                                    , role = Role "maintainer"
                                    , username = Username "bob"
                                    }
                            , tokenId =
                                TokenId
                                    "dummyTokenId"
                            }
                    }

        anti args `shouldReturn` (opts, dummyTxId)

    it "can request antithesis run" $ do
        let args =
                [ "request-test"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--username"
                , "bob"
                , "--commit"
                , "9114528e2343e6fcf3c92de71364275227e6b16d"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        UserCommand
                            { userCommand =
                                RequestTest
                                    { platform = Platform "github"
                                    , repository = Repository "cardano-foundation" "antithesis"
                                    , username = Username "bob"
                                    , commit = SHA1 "9114528e2343e6fcf3c92de71364275227e6b16d"
                                    , directory = Directory "."
                                    }
                            , tokenId =
                                TokenId
                                    "dummyTokenId"
                            }
                    }
        anti args `shouldReturn` (opts, dummyTxId)
