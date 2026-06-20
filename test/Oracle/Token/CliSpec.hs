{-# LANGUAGE OverloadedStrings #-}

module Oracle.Token.CliSpec
    ( spec
    ) where

import Cli (Command (OracleCommand))
import Control.Exception (bracket_)
import Core.Context (withContext)
import Core.Types.Basic
    ( Address (..)
    , Owner (..)
    , RequestRefId (..)
    , TokenId (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Mnemonics
    ( Mnemonics (ClearText)
    , MnemonicsPhase (DecryptedS)
    )
import Core.Types.Operation (Operation (..))
import Core.Types.Tx
    ( Root (..)
    , TxHash (..)
    , UnsignedTx (..)
    , WithTxHash (..)
    , WithUnsignedTx (..)
    )
import Core.Types.Wallet (Wallet (..))
import Core.Types.Wallet qualified as Wallet
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import Data.Version (makeVersion)
import Lib.Box (Box (..))
import MPFS.API (MPFS (..))
import MockMPFS (mockMPFS)
import Options (Options (..), parseArgs)
import Oracle.Cli (OracleCommand (..))
import Oracle.Token.Cli
    ( TokenCommand (..)
    , TokenRejectFailure (..)
    , TokenRejectRequestValidation (..)
    , TokenRejectRequestValidationProblem (..)
    , tokenCmdCore
    )
import Oracle.Types
    ( Request (..)
    , RequestZoo (..)
    , Token (..)
    , TokenState (..)
    )
import Oracle.Validate.Requests.TestRun.Lib (mkEffects, noValidation)
import Oracle.Validate.Types (AValidationResult (..))
import Submitting (Submission (..), readWallet)
import System.Environment
    ( lookupEnv
    , setEnv
    , unsetEnv
    , withArgs
    )
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    )
import Text.JSON.Canonical (JSValue (JSString), ToJSON (toJSON))

spec :: Spec
spec =
    describe "Oracle.Token.Cli" $ do
        describe "tokenCmdCore" $ do
            it "routes RejectToken through mpfsRejectTokenFromFacts" $ do
                runReject markedMPFS [wantedRef]
                    `shouldBe` ValidationSuccess (TxHash "facts-reject")

            it "rejects an empty wanted request-ref subset before submission" $ do
                runReject markedMPFS []
                    `shouldBe` ValidationFailure TokenRejectOfNoRequests

            it "rejects a missing wanted request ref before submission" $ do
                runReject markedMPFS [RequestRefId "missing-0"]
                    `shouldBe` ValidationFailure
                        ( TokenRejectRequestValidations
                            [ TokenRejectRequestValidation
                                (RequestRefId "missing-0")
                                TokenRejectRequestNotFound
                            ]
                        )

            it "rejects a non-oracle signing wallet before submission" $ do
                runRejectAs
                    nonOracleWallet
                    markedMPFS{mpfsRejectTokenFromFacts = unexpectedSubmit}
                    [wantedRef]
                    `shouldBe` ValidationFailure TokenRejectNotRequestedFromTokenOwner

            it "rejects an unparsable token before submission" $ do
                let unparsable =
                        markedMPFS
                            { mpfsGetToken = const $ pure (JSString "not-a-token")
                            , mpfsRejectTokenFromFacts = unexpectedSubmit
                            }
                runReject unparsable [wantedRef]
                    `shouldBe` ValidationFailure (TokenRejectNotParsable sampleToken)

        it "parses oracle token reject with one or more -o refs" $ do
            withSystemTempFile "wallet.json" $ \walletPath handle -> do
                hPutStr handle walletJsonContent
                hClose handle
                parsed <-
                    withParserEnv
                        $ withArgs
                            [ "oracle"
                            , "token"
                            , "reject"
                            , "-t"
                            , "token"
                            , "-w"
                            , walletPath
                            , "-o"
                            , "aaa-0"
                            , "-o"
                            , "bbb-1"
                            ]
                        $ parseArgs (makeVersion [0])
                case parsed of
                    Box
                        ( Options
                                False
                                ( OracleCommand
                                        _
                                        _
                                        ( OracleTokenCommand
                                                ( RejectToken
                                                        token
                                                        _
                                                        refs
                                                    )
                                            )
                                    )
                            ) -> do
                            token `shouldBe` sampleToken
                            refs
                                `shouldBe` [ RequestRefId "aaa#0"
                                           , RequestRefId "bbb#1"
                                           ]
                    _ -> expectationFailure "expected oracle token reject command"

sampleToken :: TokenId
sampleToken = TokenId "token"

wantedRef :: RequestRefId
wantedRef = RequestRefId "aaa-0"

sampleRequest :: RequestZoo
sampleRequest =
    UnknownInsertRequest
        Request
            { outputRefId = wantedRef
            , owner = Owner "requester"
            , change = Change (Key (JSString "key")) (Insert (JSString "value"))
            }

runReject
    :: MPFS Identity
    -> [RequestRefId]
    -> AValidationResult TokenRejectFailure TxHash
runReject = runRejectAs sampleWallet

runRejectAs
    :: Wallet
    -> MPFS Identity
    -> [RequestRefId]
    -> AValidationResult TokenRejectFailure TxHash
runRejectAs wallet mpfs wanted =
    runIdentity
        $ withContext
            mpfs
            (\m _ -> mkEffects m noValidation)
            (const recordingSubmission)
            (tokenCmdCore (RejectToken sampleToken wallet wanted))

withRequestsOwnedBy
    :: Monad m => Owner -> [RequestZoo] -> MPFS m -> MPFS m
withRequestsOwnedBy tokenOwner reqs mpfs =
    mpfs
        { mpfsGetToken =
            \_ ->
                toJSON
                    Token
                        { tokenRequests = Identity <$> reqs
                        , tokenState =
                            TokenState
                                { tokenRoot = Root "mock-root"
                                , tokenOwner = tokenOwner
                                }
                        , tokenRefId = RequestRefId "mock-token-ref-id"
                        }
        }

markedMPFS :: MPFS Identity
markedMPFS =
    withRequestsOwnedBy
        (Wallet.owner sampleWallet)
        [sampleRequest]
        mockMPFS
            { mpfsRejectTokenFromFacts =
                \_ _ _ -> pure $ marker "facts-reject"
            }

unexpectedSubmit
    :: Address
    -> TokenId
    -> [RequestRefId]
    -> Identity (WithUnsignedTx JSValue)
unexpectedSubmit _ _ _ = error "reject submission should not run"

marker :: Text -> WithUnsignedTx JSValue
marker name = WithUnsignedTx (UnsignedTx name) Nothing

recordingSubmission :: Submission Identity
recordingSubmission = Submission $ \action -> do
    WithUnsignedTx (UnsignedTx tx) value <- action (Address "addr")
    pure (WithTxHash (TxHash tx) value)

sampleWallet :: Wallet
sampleWallet = case readWallet (True, sampleMnemonics) of
    Left err -> error (show err)
    Right wallet -> wallet

nonOracleWallet :: Wallet
nonOracleWallet =
    sampleWallet{Wallet.owner = Owner "not-oracle"}

sampleMnemonics :: Mnemonics 'DecryptedS
sampleMnemonics =
    ClearText
        "coin april solid purity wish slight acquire kitchen dragon faculty clutch picnic"

walletJsonContent :: String
walletJsonContent =
    "{\"mnemonics\":\"coin april solid purity wish slight \
    \acquire kitchen dragon faculty clutch picnic\"}"

withParserEnv :: IO a -> IO a
withParserEnv =
    withEnv "MOOG_GITHUB_PAT" "token"
        . withEnv "MOOG_MPFS_HOST" "http://127.0.0.1:1"
        . withoutEnv "MOOG_WALLET_PASSPHRASE"

withEnv :: String -> String -> IO a -> IO a
withEnv name v action = do
    mPrev <- lookupEnv name
    bracket_ (setEnv name v) (restore name mPrev) action

withoutEnv :: String -> IO a -> IO a
withoutEnv name action = do
    mPrev <- lookupEnv name
    bracket_ (unsetEnv name) (restore name mPrev) action

restore :: String -> Maybe String -> IO ()
restore name Nothing = unsetEnv name
restore name (Just old) = setEnv name old
