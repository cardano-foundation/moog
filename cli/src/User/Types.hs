{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module User.Types
    ( TestRun (..)
    , TestRunState (..)
    , Duration (..)
    , Reason (..)
    , Phase (..)
    , URL (..)
    , RegisterUserKey (..)
    , RegisterRoleKey (..)
    )
where

import Control.Applicative (Alternative)
import Control.Monad (guard)
import Core.Types
    ( Directory (..)
    , Platform (..)
    , PublicKeyHash (..)
    , Repository (..)
    , SHA1 (..)
    , Username (..)
    )
import Data.Map.Strict qualified as Map
import Lib.JSON
    ( getField
    , getIntegralField
    , getListField
    , getStringField
    , getStringMapField
    , intJSON
    , object
    , stringJSON
    , (.:)
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
    , repository :: Repository
    , directory :: Directory
    , commitId :: SHA1
    , testRunIndex :: Int
    , requester :: Username
    }
    deriving (Eq, Show)

instance Monad m => ToJSON m TestRun where
    toJSON
        ( TestRun
                (Platform platform)
                (Repository owner repo)
                (Directory directory)
                (SHA1 commitId)
                testRunIndex
                (Username requester)
            ) =
            object
                [ ("platform", stringJSON platform)
                ,
                    ( "repository"
                    , object
                        [ ("organization", stringJSON owner)
                        , ("repo", stringJSON repo)
                        ]
                    )
                , ("directory", stringJSON directory)
                , ("commitId", stringJSON commitId)
                , ("round", intJSON testRunIndex)
                , ("requester", stringJSON requester)
                ]

instance (Monad m, ReportSchemaErrors m) => FromJSON m TestRun where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        platform <- getStringField "platform" mapping
        repository <- do
            repoMapping <- getStringMapField "repository" mapping
            owner <- getStringField "organization" repoMapping
            repo <- getStringField "repo" repoMapping
            pure $ Repository{organization = owner, project = repo}
        directory <- getStringField "directory" mapping
        commitId <- getStringField "commitId" mapping
        testRunIndex <- getIntegralField "round" mapping
        requester <- getStringField "requester" mapping
        pure
            $ TestRun
                { platform = Platform platform
                , repository = repository
                , directory = Directory directory
                , commitId = SHA1 commitId
                , testRunIndex = testRunIndex
                , requester = Username requester
                }
    fromJSON r =
        expectedButGotValue
            "an object representing a test run"
            r

newtype Duration = Duration Int
    deriving (Eq, Show)

data Phase = PendingT | DoneT | RunningT

newtype URL = URL String
    deriving (Show, Eq)

data Reason
    = UnacceptableDuration
    | UnacceptablePlatform
    | UnacceptableRepository
    | UnacceptableCommit
    | UnacceptableRound
    | UnacceptableRequester
    deriving (Eq, Show)

instance Monad m => ToJSON m Reason where
    toJSON UnacceptableDuration =
        stringJSON "unacceptable duration"
    toJSON UnacceptablePlatform =
        stringJSON "unacceptable platform"
    toJSON UnacceptableRepository =
        stringJSON "unacceptable repository"
    toJSON UnacceptableCommit =
        stringJSON "unacceptable commit"
    toJSON UnacceptableRound =
        stringJSON "unacceptable round"
    toJSON UnacceptableRequester =
        stringJSON "unacceptable requester"

instance ReportSchemaErrors m => FromJSON m Reason where
    fromJSON (JSString jsString) = do
        let reason = fromJSString jsString
        case reason of
            "unacceptable duration" -> pure UnacceptableDuration
            "unacceptable platform" -> pure UnacceptablePlatform
            "unacceptable repository" -> pure UnacceptableRepository
            "unacceptable commit" -> pure UnacceptableCommit
            "unacceptable round" -> pure UnacceptableRound
            "unacceptable requester" -> pure UnacceptableRequester
            _ -> expectedButGotValue "a valid reason" (JSString jsString)
    fromJSON other =
        expectedButGotValue "a string representing a reason" other

data TestRunState a where
    Pending :: Duration -> TestRunState PendingT
    Rejected :: TestRunState PendingT -> [Reason] -> TestRunState DoneT
    Accepted :: TestRunState PendingT -> TestRunState RunningT
    Finished
        :: TestRunState RunningT -> Duration -> URL -> TestRunState DoneT

deriving instance Eq (TestRunState a)
deriving instance Show (TestRunState a)
instance Monad m => ToJSON m (TestRunState a) where
    toJSON (Pending (Duration d)) =
        object
            [ ("phase", stringJSON "pending")
            , ("duration", intJSON d)
            ]
    toJSON (Rejected pending reasons) =
        object
            [ ("phase", stringJSON "rejected")
            , ("pending", toJSON pending)
            , ("reasons", traverse toJSON reasons >>= toJSON)
            ]
    toJSON (Accepted pending) =
        object
            [ ("phase", stringJSON "accepted")
            , ("pending", toJSON pending)
            ]
    toJSON (Finished running duration url) =
        object
            [ ("phase", stringJSON "finished")
            , ("running", toJSON running)
            , ("duration", intJSON $ case duration of Duration d -> d)
            , ("url", stringJSON $ case url of URL u -> u)
            ]

instance ReportSchemaErrors m => FromJSON m (TestRunState PendingT) where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        phase <- getStringField "phase" mapping
        case phase of
            "pending" -> do
                duration <- getIntegralField "duration" mapping
                pure $ Pending (Duration duration)
            _ ->
                expectedButGotValue
                    "a pending phase"
                    obj
    fromJSON other =
        expectedButGotValue
            "an object representing a pending phase"
            other

instance ReportSchemaErrors m => FromJSON m (TestRunState DoneT) where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        phase <- getStringField "phase" mapping
        case phase of
            "rejected" -> do
                pending <- getField "pending" mapping >>= fromJSON
                reasons <- getListField "reasons" mapping
                reasonList <- traverse fromJSON reasons
                pure $ Rejected pending reasonList
            "finished" -> do
                running <- getField "running" mapping >>= fromJSON
                duration <- getIntegralField "duration" mapping
                url <- getStringField "url" mapping
                pure $ Finished running (Duration duration) (URL url)
            _ ->
                expectedButGotValue
                    "a rejected phase"
                    obj
    fromJSON other =
        expectedButGotValue
            "an object representing a rejected phase"
            other

instance ReportSchemaErrors m => FromJSON m (TestRunState RunningT) where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        phase <- getStringField "phase" mapping
        case phase of
            "accepted" -> do
                pending <- getField "pending" mapping >>= fromJSON
                pure $ Accepted pending
            _ ->
                expectedButGotValue
                    "an accepted phase"
                    obj
    fromJSON other =
        expectedButGotValue
            "an object representing an accepted phase"
            other

data RegisterUserKey = RegisterUserKey
    { platform :: Platform
    , username :: Username
    , pubkeyhash :: PublicKeyHash
    }
    deriving (Eq, Show)

instance Monad m => ToJSON m RegisterUserKey where
    toJSON
        ( RegisterUserKey
                (Platform platform)
                (Username user)
                (PublicKeyHash pubkeyhash)
            ) =
            toJSON
                $ Map.fromList
                    [ ("type", JSString $ toJSString "register-user")
                    , ("platform" :: String, JSString $ toJSString platform)
                    , ("user", JSString $ toJSString user)
                    , ("publickeyhash", JSString $ toJSString pubkeyhash)
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
        pubkeyhash <- getStringField "publickeyhash" mapping
        pure
            $ RegisterUserKey
                { platform = Platform platform
                , username = Username user
                , pubkeyhash = PublicKeyHash pubkeyhash
                }
    fromJSON r =
        expectedButGotValue
            "an object representing an accepted phase"
            r

data RegisterRoleKey = RegisterRoleKey
    { platform :: Platform
    , repository :: Repository
    , username :: Username
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
            pure $ Repository{organization = owner, project = repo}
        user <- mapping .: "user"
        pure
            $ RegisterRoleKey
                { platform = Platform platform
                , repository = repository
                , username = Username user
                }
    fromJSON r =
        expectedButGotValue
            "an object representing a register role"
            r

instance Monad m => ToJSON m RegisterRoleKey where
    toJSON
        ( RegisterRoleKey
                (Platform platform)
                (Repository owner repo)
                (Username user)
            ) =
            object
                [ ("type", stringJSON "register-role")
                , ("platform", stringJSON platform)
                ,
                    ( "repository"
                    , object
                        [ ("organization", stringJSON owner)
                        , ("project", stringJSON repo)
                        ]
                    )
                , ("user", stringJSON user)
                ]
