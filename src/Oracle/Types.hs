{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Oracle.Types
    ( Request (..)
    , Token (..)
    , TokenState (..)
    , RequestZoo (..)
    , requestZooRefId
    , fmapToken
    , fmapMToken
    , requestZooGetRegisterUserKey
    , requestZooGetRegisterRoleKey
    , requestZooGetTestRunKey
    ) where

import Control.Applicative (Alternative, (<|>))
import Core.Types.Basic (Owner, RequestRefId)
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation (Op (..), Operation (..))
import Core.Types.Tx (Root)
import Data.Functor.Identity (Identity (..))
import Lib.JSON.Canonical.Extra (object, withObject, (.:), (.=))
import Oracle.Config.Types (Config, ConfigKey)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue
    , ReportSchemaErrors
    , ToJSON (..)
    )
import User.Agent.Types (WhiteListKey)
import User.Types
    ( Phase (..)
    , RegisterRoleKey
    , RegisterUserKey
    , TestRun
    , TestRunState
    )

data Request k op = Request
    { outputRefId :: RequestRefId
    , owner :: Owner
    , change :: Change k op
    }

deriving instance (Show k, Show (Operation op)) => Show (Request k op)
deriving instance (Eq k, Eq (Operation op)) => Eq (Request k op)

instance
    (Monad m, ToJSON m k, ToJSON m (Operation op))
    => ToJSON m (Request k op)
    where
    toJSON (Request refId owner change) =
        object
            [ "outputRefId" .= refId
            , "owner" .= owner
            , "change" .= change
            ]

instance
    (ReportSchemaErrors m, FromJSON m k, FromJSON m (Operation op))
    => FromJSON m (Request k op)
    where
    fromJSON = withObject "Request" $ \v -> do
        refId <- v .: "outputRefId"
        owner <- v .: "owner"
        change <- v .: "change"
        pure
            $ Request
                { outputRefId = refId
                , owner = owner
                , change = change
                }

data TokenState = TokenState
    { tokenRoot :: Root
    , tokenOwner :: Owner
    }
    deriving (Eq, Show)

instance Monad m => ToJSON m TokenState where
    toJSON (TokenState root owner) =
        object
            [ "root" .= root
            , "owner" .= owner
            ]

instance ReportSchemaErrors m => FromJSON m TokenState where
    fromJSON = withObject "TokenState" $ \v -> do
        root <- v .: "root"
        owner <- v .: "owner"
        pure $ TokenState{tokenRoot = root, tokenOwner = owner}

data RequestZoo where
    RegisterUserRequest
        :: Request RegisterUserKey (OpI ()) -> RequestZoo
    UnregisterUserRequest
        :: Request RegisterUserKey (OpD ()) -> RequestZoo
    RegisterRoleRequest
        :: Request RegisterRoleKey (OpI ()) -> RequestZoo
    UnregisterRoleRequest
        :: Request RegisterRoleKey (OpD ()) -> RequestZoo
    CreateTestRequest
        :: Request TestRun (OpI (TestRunState PendingT)) -> RequestZoo
    RejectRequest
        :: Request TestRun (OpU (TestRunState PendingT) (TestRunState DoneT))
        -> RequestZoo
    AcceptRequest
        :: Request TestRun (OpU (TestRunState PendingT) (TestRunState RunningT))
        -> RequestZoo
    FinishedRequest
        :: Request TestRun (OpU (TestRunState RunningT) (TestRunState DoneT))
        -> RequestZoo
    WhiteListRequest
        :: Request WhiteListKey (OpI ()) -> RequestZoo
    BlackListRequest
        :: Request WhiteListKey (OpD ()) -> RequestZoo
    InsertConfigRequest
        :: Request ConfigKey (OpI Config) -> RequestZoo
    UpdateConfigRequest
        :: Request ConfigKey (OpU Config Config) -> RequestZoo
    UnknownInsertRequest
        :: Request JSValue (OpI JSValue) -> RequestZoo
    UnknownDeleteRequest
        :: Request JSValue (OpD JSValue) -> RequestZoo
    UnknownUpdateRequest
        :: Request JSValue (OpU JSValue JSValue) -> RequestZoo
    deriving (Show, Eq)

requestZooRefId :: RequestZoo -> RequestRefId
requestZooRefId (RegisterUserRequest req) = outputRefId req
requestZooRefId (UnregisterUserRequest req) = outputRefId req
requestZooRefId (RegisterRoleRequest req) = outputRefId req
requestZooRefId (UnregisterRoleRequest req) = outputRefId req
requestZooRefId (CreateTestRequest req) = outputRefId req
requestZooRefId (RejectRequest req) = outputRefId req
requestZooRefId (AcceptRequest req) = outputRefId req
requestZooRefId (FinishedRequest req) = outputRefId req
requestZooRefId (WhiteListRequest req) = outputRefId req
requestZooRefId (BlackListRequest req) = outputRefId req
requestZooRefId (InsertConfigRequest req) = outputRefId req
requestZooRefId (UpdateConfigRequest req) = outputRefId req
requestZooRefId (UnknownInsertRequest req) = outputRefId req
requestZooRefId (UnknownDeleteRequest req) = outputRefId req
requestZooRefId (UnknownUpdateRequest req) = outputRefId req

requestZooGetRegisterUserKey :: RequestZoo -> Maybe RegisterUserKey
requestZooGetRegisterUserKey
    (RegisterUserRequest (Request _ _ (Change (Key k) _))) = Just k
requestZooGetRegisterUserKey
    (UnregisterUserRequest (Request _ _ (Change (Key k) _))) = Just k
requestZooGetRegisterUserKey _ = Nothing

requestZooGetRegisterRoleKey :: RequestZoo -> Maybe RegisterRoleKey
requestZooGetRegisterRoleKey
    (RegisterRoleRequest (Request _ _ (Change (Key k) _))) = Just k
requestZooGetRegisterRoleKey
    (UnregisterRoleRequest (Request _ _ (Change (Key k) _))) = Just k
requestZooGetRegisterRoleKey _ = Nothing

requestZooGetTestRunKey :: RequestZoo -> Maybe TestRun
requestZooGetTestRunKey
    (CreateTestRequest (Request _ _ (Change (Key k) _))) = Just k
requestZooGetTestRunKey
    (RejectRequest (Request _ _ (Change (Key k) _))) = Just k
requestZooGetTestRunKey
    (AcceptRequest (Request _ _ (Change (Key k) _))) = Just k
requestZooGetTestRunKey
    (FinishedRequest (Request _ _ (Change (Key k) _))) = Just k
requestZooGetTestRunKey _ = Nothing

instance (Alternative m, ReportSchemaErrors m) => FromJSON m RequestZoo where
    fromJSON v = do
        (RegisterUserRequest <$> fromJSON v)
            <|> (UnregisterUserRequest <$> fromJSON v)
            <|> (RegisterRoleRequest <$> fromJSON v)
            <|> (UnregisterRoleRequest <$> fromJSON v)
            <|> (CreateTestRequest <$> fromJSON v)
            <|> (RejectRequest <$> fromJSON v)
            <|> (AcceptRequest <$> fromJSON v)
            <|> (FinishedRequest <$> fromJSON v)
            <|> (WhiteListRequest <$> fromJSON v)
            <|> (BlackListRequest <$> fromJSON v)
            <|> (InsertConfigRequest <$> fromJSON v)
            <|> (UpdateConfigRequest <$> fromJSON v)
            <|> (UnknownInsertRequest <$> fromJSON v)
            <|> (UnknownDeleteRequest <$> fromJSON v)
            <|> (UnknownUpdateRequest <$> fromJSON v)

instance Monad m => ToJSON m RequestZoo where
    toJSON (RegisterUserRequest req) = toJSON req
    toJSON (UnregisterUserRequest req) = toJSON req
    toJSON (RegisterRoleRequest req) = toJSON req
    toJSON (UnregisterRoleRequest req) = toJSON req
    toJSON (CreateTestRequest req) = toJSON req
    toJSON (RejectRequest req) = toJSON req
    toJSON (AcceptRequest req) = toJSON req
    toJSON (FinishedRequest req) = toJSON req
    toJSON (WhiteListRequest req) = toJSON req
    toJSON (BlackListRequest req) = toJSON req
    toJSON (InsertConfigRequest req) = toJSON req
    toJSON (UpdateConfigRequest req) = toJSON req
    toJSON (UnknownInsertRequest req) = toJSON req
    toJSON (UnknownDeleteRequest req) = toJSON req
    toJSON (UnknownUpdateRequest req) = toJSON req

data Token r = Token
    { tokenRefId :: RequestRefId
    , tokenState :: TokenState
    , tokenRequests :: [r RequestZoo]
    }

deriving instance Show (Token Identity)
deriving instance Eq (Token Identity)

fmapToken :: (r RequestZoo -> r' RequestZoo) -> Token r -> Token r'
fmapToken f (Token refId state requests) =
    Token
        { tokenRefId = refId
        , tokenState = state
        , tokenRequests = f <$> requests
        }

fmapMToken
    :: (Monad m)
    => (r RequestZoo -> m (r' RequestZoo))
    -> Token r
    -> m (Token r')
fmapMToken f (Token refId state requests) = do
    requests' <- mapM f requests
    pure
        $ Token
            { tokenRefId = refId
            , tokenState = state
            , tokenRequests = requests'
            }

instance (Monad m, ToJSON m (r RequestZoo)) => ToJSON m (Token r) where
    toJSON (Token refId state requests) =
        object
            [ "outputRefId" .= refId
            , "state" .= state
            , "requests" .= requests
            ]

instance
    (Alternative m, ReportSchemaErrors m)
    => FromJSON m (Token Identity)
    where
    fromJSON = withObject "Token" $ \v -> do
        refId <- v .: "outputRefId"
        state <- v .: "state"
        requests <- v .: "requests"
        pure
            $ Token
                { tokenRefId = refId
                , tokenState = state
                , tokenRequests = requests
                }
