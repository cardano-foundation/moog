module User.Agent.Types
    ( TestRunStatus (..)
    , TestRunMap (..)
    , WhiteListKey (..)
    , TestRunId (..)
    , mkTestRunId
    , testRunIdFromFact
    ) where

import Control.Monad (unless)
import Core.Types.Basic (GithubRepository, Platform)
import Core.Types.Fact (Fact (..), keyHash)
import Data.Functor.Identity (Identity (..))
import Lib.JSON.Canonical.Extra (object, withObject, (.:), (.=))
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors
    , ToJSON (..)
    , expectedButGotValue
    , toJSString
    )
import User.Types (Phase (..), TestRun, TestRunState)

newtype TestRunId = TestRunId {testRunId :: String}
    deriving (Show, Eq)

mkTestRunId :: ToJSON Identity key => key -> TestRunId
mkTestRunId = TestRunId . runIdentity . keyHash

testRunIdFromFact :: Fact TestRun v -> TestRunId
testRunIdFromFact = mkTestRunId . factKey
instance Monad m => ToJSON m TestRunId where
    toJSON (TestRunId hash) =
        pure $ JSString $ toJSString hash

instance (ReportSchemaErrors m) => FromJSON m TestRunId where
    fromJSON i = do
        str <- fromJSON @m @String i
        pure $ TestRunId str

data WhiteListKey = WhiteListKey
    { platform :: Platform
    , repository :: GithubRepository
    }
    deriving (Show, Eq)

instance Monad m => ToJSON m WhiteListKey where
    toJSON (WhiteListKey p r) =
        object
            [ "repository" .= r
            , "platform" .= p
            , "type" .= ("white-list-repo" :: String)
            ]

instance (ReportSchemaErrors m) => FromJSON m WhiteListKey where
    fromJSON = withObject "WhiteListKey" $ \v -> do
        t <- v .: "type"
        unless (t == ("white-list-repo" :: String))
            $ expectedButGotValue
                "white-list-repo"
            $ JSString
            $ toJSString t
        repository <- v .: "repository"
        platform <- v .: "platform"
        pure $ WhiteListKey platform repository

data TestRunStatus phase where
    StatusPending
        :: Fact TestRun (TestRunState PendingT) -> TestRunStatus PendingT
    StatusRunning
        :: Fact TestRun (TestRunState RunningT) -> TestRunStatus RunningT
    StatusDone :: Fact TestRun (TestRunState DoneT) -> TestRunStatus DoneT

deriving instance Show (TestRunStatus phase)

deriving instance Eq (TestRunStatus phase)

instance Monad m => ToJSON m (TestRunStatus phase) where
    toJSON (StatusPending fact) = toJSON fact
    toJSON (StatusRunning fact) = toJSON fact
    toJSON (StatusDone fact) = toJSON fact

data TestRunMap = TestRunMap
    { pending :: [TestRunStatus PendingT]
    , running :: [TestRunStatus RunningT]
    , done :: [TestRunStatus DoneT]
    }
    deriving (Show, Eq)

instance Monad m => ToJSON m TestRunMap where
    toJSON (TestRunMap p r d) =
        object
            [ "pending" .= p
            , "running" .= r
            , "done" .= d
            ]
