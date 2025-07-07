{-# LANGUAGE QuasiQuotes #-}

module CliSpec
    ( spec
    , runDummyServer
    , anti
    )
where

import App (client)
import Cli (Command (..))
import Core.Types
    ( Directory (..)
    , Platform (..)
    , PublicKeyHash (..)
    , Repository (..)
    , RequestRefId (..)
    , SHA1 (..)
    , Username (..)
    )
import Data.Aeson (encodeFile)
import Data.Aeson.QQ (aesonQQ)
import Lib.Box (Box (..))
import Lib.JSON (object, (.=))
import Options (Options (..))
import Oracle.Cli (OracleCommand (..))
import Oracle.Token.Cli (TokenCommand (..))
import System.Environment (setEnv, withArgs)
import Test.Hspec
    ( Spec
    , beforeAll_
    , shouldReturn
    , xit
    )
import Text.JSON.Canonical (Int54, JSValue (JSArray, JSNull))
import User.Requester.Cli (RequesterCommand (..))
import User.Types
    ( Duration (..)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    )

runDummyServer :: IO ()
runDummyServer = do
    let walletFile = "/tmp/anti-test-wallet.json"
    encodeFile
        walletFile
        [aesonQQ|
        {
            "mnemonic": [
                "very",
                "actress",
                "black",
                "another",
                "choice",
                "cry",
                "consider",
                "agree",
                "sudden",
                "garage",
                "error",
                "transfer"
            ],
            "address":
                "addr_test1vp46vqqjjw0d9hdjj26musy6udyph3jx55rdvepvrke4p6qsd5vhd"
        }
        |]

    setEnv "ANTI_MPFS_HOST" "http://localhost:8084"
    setEnv "ANTI_TOKEN_ID" "dummyTokenId"
    setEnv "ANTI_WALLET_FILE" walletFile
    return ()

anti :: [String] -> IO (Box Options, JSValue)
anti args = do
    -- Simulate the command line arguments
    -- Call the main function with the simulated arguments
    ev <- withArgs args client
    case ev of
        (_, _, _, Left err) -> error $ "Error: " ++ show err
        (o, _, _, Right result) -> return (o, result)

dummyTxHash :: Monad m => m JSValue
dummyTxHash =
    object
        ["txHash" .= ("dummyTxHash" :: String), "value" .= JSNull]

spec :: Spec
spec = beforeAll_ runDummyServer $ do
    dummyTxHashJSON <- dummyTxHash
    xit "can request user registration" $ do
        let args =
                [ "requester"
                , "register-user"
                , "--platform"
                , "github"
                , "--username"
                , "paolino"
                , "--pubkeyhash"
                , "AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8"
                ]
        let opts =
                Box
                    $ Options
                        { optionsCommand =
                            RequesterCommand
                                $ RegisterUser
                                    RegisterUserKey
                                        { platform = Platform "github"
                                        , username = Username "paolino"
                                        , pubkeyhash =
                                            PublicKeyHash
                                                "AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8"
                                        }
                        }

        anti args
            `shouldReturn` (opts, dummyTxHashJSON)
    xit "can request user unregistration" $ do
        let args =
                [ "requester"
                , "unregister-user"
                , "--platform"
                , "github"
                , "--username"
                , "bob"
                , "--pubkeyhash"
                , "607a0d8a64616a407537edf0d9b59cf4cb509c556f6d2de4250ce15df2"
                ]

        let opts =
                Box
                    $ Options
                        { optionsCommand =
                            RequesterCommand
                                $ UnregisterUser
                                    RegisterUserKey
                                        { platform = Platform "github"
                                        , username = Username "bob"
                                        , pubkeyhash =
                                            PublicKeyHash
                                                "607a0d8a64616a407537edf0d9b59cf4cb509c556f6d2de4250ce15df2"
                                        }
                        }
        anti args `shouldReturn` (opts, dummyTxHashJSON)

    xit "can request removing user from a project" $ do
        let args =
                [ "requester"
                , "unregister-role"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--username"
                , "bob"
                ]
        let opts =
                Box
                    $ Options
                        { optionsCommand =
                            RequesterCommand
                                $ RegisterRole
                                $ RegisterRoleKey
                                    { platform = Platform "github"
                                    , repository = Repository "cardano-foundation" "antithesis"
                                    , username = Username "bob"
                                    }
                        }

        anti args `shouldReturn` (opts, dummyTxHashJSON)

    xit "can request antithesis run" $ do
        let args =
                [ "requester"
                , "create-test"
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
                Box
                    $ Options
                        { optionsCommand =
                            RequesterCommand
                                $ RequestTest
                                    TestRun
                                        { platform = Platform "github"
                                        , repository = Repository "cardano-foundation" "antithesis"
                                        , requester = Username "bob"
                                        , commitId = SHA1 "9114528e2343e6fcf3c92de71364275227e6b16d"
                                        , directory = Directory "."
                                        , testRunIndex = 1
                                        }
                                $ Duration 3
                        }

        result <-
            object
                [ "txHash" .= ("dummyTxHash" :: String)
                , (,) "value"
                    $ object
                        [ "duration" .= (3 :: Int54)
                        , "phase" .= ("pending" :: String)
                        ]
                ]

        anti args `shouldReturn` (opts, result)

    xit "can retract a request" $ do
        let args =
                [ "retract"
                , "--outref"
                , "9114528e2343e6fcf3c92de71364275227e6b16d-0"
                ]
        let opts =
                Box
                    $ Options
                        { optionsCommand =
                            RetractRequest
                                { outputReference =
                                    RequestRefId
                                        "9114528e2343e6fcf3c92de71364275227e6b16d-0"
                                }
                        }
        anti args `shouldReturn` (opts, dummyTxHashJSON)

    xit "can get a token" $ do
        let args =
                [ "oracle"
                , "token"
                , "get"
                ]
        let opts =
                Box
                    $ Options
                        { optionsCommand =
                            OracleCommand $ OracleTokenCommand GetToken
                        }
        anti args
            `shouldReturn` ( opts
                           , JSArray []
                           )
