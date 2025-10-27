{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module User.Types
    ( TestRun (..)
    , platformL
    , repositoryL
    , directoryL
    , commitIdL
    , tryIndexL
    , requesterL
    , Outcome (..)
    , TestRunState (..)
    , TestRunRejection (..)
    , Phase (..)
    , URL (..)
    , RegisterUserKey (..)
    , GithubIdentification (..)
    , RegisterRoleKey (..)
    , roleOfATestRun
    , AgentValidation (..)
    , getIdentificationPublicKey
    )
where

import Control.Applicative (Alternative, (<|>))
import Control.Lens (makeLensesFor)
import Control.Monad (guard)
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , Duration (..)
    , GithubRepository (..)
    , GithubUsername (..)
    , Platform (..)
    , Try (..)
    )
import Core.Types.VKey (decodeVKey)
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteArray qualified as BA
import Data.CaseInsensitive (CI (..), mk)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effects.RegisterUser (VKey (..))
import Lib.JSON.Canonical.Extra
    ( byteStringFromJSON
    , byteStringToJSON
    , getField
    , getIntegralField
    , getListField
    , getStringField
    , getStringMapField
    , intJSON
    , object
    , stringJSON
    , (.:)
    , (.=)
    )
import Lib.SSH.Public
    ( SSHPublicKey
    , decodeSSHPublicKey
    , makeSSHPublicKey
    , unmakeSSHPublicKey
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors
    , ToJSON (..)
    , expectedButGotValue
    , fromJSString
    , toJSString
    )

data TestRun = TestRun
    { platform :: Platform
    , repository :: GithubRepository
    , directory :: Directory
    , commitId :: Commit
    , tryIndex :: Try
    , requester :: GithubUsername
    }
    deriving (Eq, Show)

makeLensesFor
    [ ("platform", "platformL")
    , ("repository", "repositoryL")
    , ("directory", "directoryL")
    , ("commitId", "commitIdL")
    , ("tryIndex", "tryIndexL")
    , ("requester", "requesterL")
    ]
    ''TestRun

roleOfATestRun :: TestRun -> RegisterRoleKey
roleOfATestRun
    TestRun
        { platform
        , repository
        , requester = username
        } =
        RegisterRoleKey
            { platform
            , repository
            , username
            }

instance (Monad m) => ToJSON m TestRun where
    toJSON
        ( TestRun
                platform
                repository
                (Directory directory)
                (Commit commitId)
                (Try tryIndex)
                (GithubUsername requester)
            ) =
            object
                [ ("type", stringJSON "test-run")
                , "platform" .= platform
                , "repository" .= repository
                , ("directory", stringJSON directory)
                , ("commitId", stringJSON commitId)
                , ("try", intJSON tryIndex)
                , ("requester", stringJSON $ foldedCase requester)
                ]

instance (Monad m, ReportSchemaErrors m) => FromJSON m TestRun where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        requestType <- mapping .: "type"
        case requestType of
            JSString "test-run" -> do
                platform <- getStringField "platform" mapping
                repository <- do
                    repoMapping <- getStringMapField "repository" mapping
                    owner <- getStringField "organization" repoMapping
                    repo <- getStringField "repo" repoMapping
                    pure
                        $ GithubRepository
                            { organization = mk owner
                            , project = mk repo
                            }
                directory <- getStringField "directory" mapping
                commitId <- getStringField "commitId" mapping
                tryIndex <- getIntegralField "try" mapping
                requester <- getStringField "requester" mapping
                pure
                    $ TestRun
                        { platform = Platform platform
                        , repository = repository
                        , directory = Directory directory
                        , commitId = Commit commitId
                        , tryIndex = Try tryIndex
                        , requester = GithubUsername $ mk requester
                        }
            _ ->
                expectedButGotValue
                    "a test-run type tagged request"
                    obj
    fromJSON r =
        expectedButGotValue
            "an object representing a test run"
            r

data Phase = PendingT | DoneT | RunningT

data TestRunRejection
    = BrokenInstructions
    | UnclearIntent
    deriving (Eq, Show)

instance (Monad m) => ToJSON m TestRunRejection where
    toJSON BrokenInstructions =
        stringJSON "broken instructions"
    toJSON UnclearIntent =
        stringJSON "unclear intent"

instance (ReportSchemaErrors m) => FromJSON m TestRunRejection where
    fromJSON (JSString jsString) = do
        let reason = fromJSString jsString
        case reason of
            "broken instructions" -> pure BrokenInstructions
            "unclear intent" -> pure UnclearIntent
            _ ->
                expectedButGotValue
                    "a known test run rejection reason"
                    (JSString jsString)
    fromJSON other =
        expectedButGotValue
            "a string representing a test run rejection reason"
            other

newtype URL = URL String
    deriving (Show, Eq)

data TestRunState a where
    Pending :: Duration -> Ed25519.Signature -> TestRunState PendingT
    Rejected
        :: TestRunState PendingT -> [TestRunRejection] -> TestRunState DoneT
    Accepted :: TestRunState PendingT -> TestRunState RunningT
    Finished
        :: TestRunState RunningT -> Duration -> Outcome -> URL -> TestRunState DoneT

deriving instance Eq (TestRunState a)

deriving instance Show (TestRunState a)

instance (Monad m) => ToJSON m (TestRunState a) where
    toJSON (Pending (Duration d) signature) =
        object
            [ ("phase", stringJSON "pending")
            , ("duration", intJSON d)
            , ("signature", byteStringToJSON $ BA.convert signature)
            ]
    toJSON (Rejected pending reasons) =
        object
            [ ("phase", stringJSON "rejected")
            , ("from", toJSON pending)
            , ("reasons", traverse toJSON reasons >>= toJSON)
            ]
    toJSON (Accepted pending) =
        object
            [ ("phase", stringJSON "accepted")
            , ("from", toJSON pending)
            ]
    toJSON (Finished running duration outcome url) =
        object
            [ ("phase", stringJSON "finished")
            , ("from", toJSON running)
            , ("duration", intJSON $ case duration of Duration d -> d)
            , ("url", stringJSON $ case url of URL u -> u)
            , ("outcome", toJSON outcome)
            ]

instance (ReportSchemaErrors m) => FromJSON m (TestRunState PendingT) where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        phase <- getStringField "phase" mapping
        case phase of
            "pending" -> do
                duration <- getIntegralField "duration" mapping
                signatureJSValue <- getField "signature" mapping
                signatureByteString <- byteStringFromJSON signatureJSValue
                case Ed25519.signature signatureByteString of
                    CryptoPassed signature ->
                        pure $ Pending (Duration duration) signature
                    CryptoFailed e ->
                        expectedButGotValue
                            ("a valid Ed25519 signature " ++ show e)
                            signatureJSValue
            _ ->
                expectedButGotValue
                    "a pending phase"
                    obj
    fromJSON other =
        expectedButGotValue
            "an object representing a pending phase"
            other

instance (ReportSchemaErrors m) => FromJSON m (TestRunState DoneT) where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        phase <- getStringField "phase" mapping
        case phase of
            "rejected" -> do
                pending <- getField "from" mapping >>= fromJSON
                reasons <- getListField "reasons" mapping
                reasonList <- traverse fromJSON reasons
                pure $ Rejected pending reasonList
            "finished" -> do
                running <- getField "from" mapping >>= fromJSON
                duration <- getIntegralField "duration" mapping
                url <- getStringField "url" mapping
                outcome <- getField "outcome" mapping >>= fromJSON
                pure $ Finished running (Duration duration) outcome (URL url)
            _ ->
                expectedButGotValue
                    "a rejected phase"
                    obj
    fromJSON other =
        expectedButGotValue
            "an object representing a rejected phase"
            other

instance (ReportSchemaErrors m) => FromJSON m (TestRunState RunningT) where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        phase <- getStringField "phase" mapping
        case phase of
            "accepted" -> do
                pending <- getField "from" mapping >>= fromJSON
                pure $ Accepted pending
            _ ->
                expectedButGotValue
                    "an accepted phase"
                    obj
    fromJSON other =
        expectedButGotValue
            "an object representing an accepted phase"
            other

data GithubIdentification
    = IdentifyViaSSHKey SSHPublicKey
    | IdentifyViaVKey VKey
    deriving (Eq, Show)

getIdentificationPublicKey
    :: GithubIdentification -> Maybe Ed25519.PublicKey
getIdentificationPublicKey (IdentifyViaSSHKey sshKey) = decodeSSHPublicKey sshKey
getIdentificationPublicKey (IdentifyViaVKey vKey) = case decodeVKey vKey of
    Left _ -> Nothing
    Right pk -> Just pk

instance Monad m => ToJSON m GithubIdentification where
    toJSON (IdentifyViaSSHKey (unmakeSSHPublicKey -> sshPk)) =
        object ["identifyViaSSHKey" .= sshPk]
    toJSON (IdentifyViaVKey (VKey vKey)) =
        object ["identifyViaVKey" .= vKey]

data RegisterUserKey = RegisterUserKey
    { platform :: Platform
    , username :: GithubUsername
    , githubIdentification :: GithubIdentification
    }
    deriving (Eq, Show)

instance (Monad m) => ToJSON m RegisterUserKey where
    toJSON
        ( RegisterUserKey
                (Platform platform)
                (GithubUsername user)
                (IdentifyViaSSHKey sshPk)
            ) =
            toJSON
                $ Map.fromList
                    [ ("type", JSString $ toJSString "register-user")
                    , ("platform" :: String, JSString $ toJSString platform)
                    , ("user", JSString $ toJSString $ foldedCase user)
                    ,
                        ( "publickeyhash"
                        , JSString
                            $ toJSString
                            $ unmakeSSHPublicKey sshPk
                        )
                    ]
    toJSON
        ( RegisterUserKey
                (Platform platform)
                (GithubUsername user)
                (IdentifyViaVKey (VKey vkey))
            ) =
            toJSON
                $ Map.fromList
                    [ ("type", JSString $ toJSString "register-user")
                    , ("platform" :: String, JSString $ toJSString platform)
                    , ("user", JSString $ toJSString $ foldedCase user)
                    , ("vkey", JSString $ toJSString $ T.unpack vkey)
                    ]

instance
    (Alternative m, Monad m, ReportSchemaErrors m)
    => FromJSON m RegisterUserKey
    where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        requestType <- mapping .: "type"
        guard $ requestType == JSString "register-user"
        platform <- getStringField "platform" mapping
        user <- getStringField "user" mapping
        githubIdentification <-
            IdentifyViaSSHKey . makeSSHPublicKey
                <$> getStringField "publickeyhash" mapping
                <|> IdentifyViaVKey . VKey . T.pack <$> getStringField "vkey" mapping
        pure
            $ RegisterUserKey
                { platform = Platform platform
                , username = GithubUsername $ mk user
                , githubIdentification = githubIdentification
                }
    fromJSON r =
        expectedButGotValue
            "an object representing an accepted phase"
            r

data RegisterRoleKey = RegisterRoleKey
    { platform :: Platform
    , repository :: GithubRepository
    , username :: GithubUsername
    }
    deriving (Eq, Show)

instance (ReportSchemaErrors m, Alternative m) => FromJSON m RegisterRoleKey where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        requestType <- mapping .: "type"
        guard $ requestType == JSString "register-role"
        platform <- mapping .: "platform"
        repository <- do
            repoMapping <- mapping .: "repository"
            owner <- repoMapping .: "organization"
            repo <- repoMapping .: "project"
            pure $ GithubRepository{organization = mk owner, project = mk repo}
        user <- mapping .: "user"
        pure
            $ RegisterRoleKey
                { platform = Platform platform
                , repository = repository
                , username = GithubUsername $ mk user
                }
    fromJSON r =
        expectedButGotValue
            "an object representing a register role"
            r

instance (Monad m) => ToJSON m RegisterRoleKey where
    toJSON
        ( RegisterRoleKey
                (Platform platform)
                (GithubRepository owner repo)
                (GithubUsername user)
            ) =
            object
                [ ("type", stringJSON "register-role")
                , ("platform", stringJSON platform)
                ,
                    ( "repository"
                    , object
                        [ ("organization", stringJSON $ foldedCase owner)
                        , ("project", stringJSON $ foldedCase repo)
                        ]
                    )
                , ("user", stringJSON $ foldedCase user)
                ]

data AgentValidation = AgentValidation
    { testRun :: TestRun
    , validation :: Maybe [TestRunRejection]
    }

instance (Monad m) => ToJSON m AgentValidation where
    toJSON AgentValidation{testRun, validation} =
        object
            [ ("testRun", toJSON testRun)
            ,
                ( "validation"
                , maybe (pure JSNull) toJSON validation
                )
            ]

instance (ReportSchemaErrors m) => FromJSON m AgentValidation where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        testRun <- mapping .: "testRun"
        validation <- mapping .: "validation"
        reasons <- case validation of
            JSNull -> pure Nothing
            _ -> Just <$> fromJSON validation
        pure $ AgentValidation{testRun = testRun, validation = reasons}
    fromJSON r =
        expectedButGotValue
            "an object representing an agent validation"
            r

data Outcome
    = OutcomeSuccess
    | OutcomeFailure
    | OutcomeUnknown -- Failed to parse email
    deriving (Eq, Show)

instance Monad m => ToJSON m Outcome where
    toJSON OutcomeSuccess = stringJSON "success"
    toJSON OutcomeFailure = stringJSON "failure"
    toJSON OutcomeUnknown = stringJSON "unknown"

instance ReportSchemaErrors m => FromJSON m Outcome where
    fromJSON (JSString "success") = pure OutcomeSuccess
    fromJSON (JSString "failure") = pure OutcomeFailure
    fromJSON (JSString "unknown") = pure OutcomeUnknown
    fromJSON v = expectedButGotValue "success, failure or unknown" v

