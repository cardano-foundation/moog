{-# LANGUAGE StrictData #-}

module Oracle.Types
    ( Request (..)
    , Token (..)
    , TokenState (..)
    , RequestZoo (..)
    ) where

import Control.Applicative (Alternative, (<|>))
import Core.Types
    ( Change (..)
    , Operation (..)
    , Owner
    , RequestRefId
    , Root
    )
import Lib.JSON
import Text.JSON.Canonical
import User.Types
    ( Phase (..)
    , RegisterRoleKey
    , RegisterUserKey
    , TestRun
    , TestRunState
    )

data Request k v = Request
    { outputRefId :: RequestRefId
    , owner :: Owner
    , change :: Change k v
    }
    deriving (Show, Eq)

instance (Monad m, ToJSON m k, ToJSON m v) => ToJSON m (Request k v) where
    toJSON (Request refId owner change) =
        object
            [ "outputRefId" .= refId
            , "owner" .= owner
            , "change" .= change
            ]

instance
    (ReportSchemaErrors m, FromJSON m k, FromJSON m v)
    => FromJSON m (Request k v)
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
        :: Request RegisterUserKey () -> RequestZoo
    UnregisterUserRequest
        :: Request RegisterUserKey () -> RequestZoo
    RegisterRoleRequest
        :: Request RegisterRoleKey () -> RequestZoo
    UnregisterRoleRequest
        :: Request RegisterRoleKey () -> RequestZoo
    CreateTestRequest
        :: Request TestRun (TestRunState PendingT) -> RequestZoo
    RejectRequest
        :: Request TestRun (TestRunState DoneT) -> RequestZoo
    AcceptRequest
        :: Request TestRun (TestRunState RunningT) -> RequestZoo
    FinishedRequest
        :: Request TestRun (TestRunState DoneT) -> RequestZoo

parseRegisterUserKey
    :: (ReportSchemaErrors m, Alternative m) => JSValue -> m RequestZoo
parseRegisterUserKey v = do
    r@(Request{change}) <- fromJSON v
    case operation change of
        Insert _ -> pure $ RegisterUserRequest r
        Delete _ -> pure $ UnregisterUserRequest r
        Update _ _ -> expected "insert or delete" $ Just "update"

parseRegisterRoleKey
    :: (Alternative m, ReportSchemaErrors m) => JSValue -> m RequestZoo
parseRegisterRoleKey v = do
    r@(Request{change}) <- fromJSON v
    case operation change of
        Insert _ -> pure $ RegisterRoleRequest r
        Delete _ -> pure $ UnregisterRoleRequest r
        Update _ _ -> expected "insert or delete" $ Just "update"

instance (Alternative m, ReportSchemaErrors m) => FromJSON m RequestZoo where
    fromJSON v = do
        parseRegisterUserKey v <|> parseRegisterRoleKey v

instance Monad m => ToJSON m RequestZoo where
    toJSON (RegisterUserRequest req) =
        object
            [ "type" .= ("register-user" :: String)
            , "request" .= req
            ]
    toJSON (UnregisterUserRequest req) =
        object
            [ "type" .= ("unregister-user" :: String)
            , "request" .= req
            ]
    toJSON (RegisterRoleRequest req) =
        object
            [ "type" .= ("register-role" :: String)
            , "request" .= req
            ]
    toJSON (UnregisterRoleRequest req) =
        object
            [ "type" .= ("unregister-role" :: String)
            , "request" .= req
            ]
    toJSON (CreateTestRequest req) =
        object
            [ "type" .= ("create-test" :: String)
            , "request" .= req
            ]
    toJSON (RejectRequest req) =
        object
            [ "type" .= ("reject-test" :: String)
            , "request" .= req
            ]
    toJSON (AcceptRequest req) =
        object
            [ "type" .= ("accept-test" :: String)
            , "request" .= req
            ]
    toJSON (FinishedRequest req) =
        object
            [ "type" .= ("finished-test" :: String)
            , "request" .= req
            ]

data Token = Token
    { tokenRefId :: RequestRefId
    , tokenState :: TokenState
    , tokenRequests :: [RequestZoo]
    }

instance Monad m => ToJSON m Token where
    toJSON (Token refId state requests) =
        object
            [ "outputRefId" .= refId
            , "state" .= state
            , "requests" .= requests
            ]

instance
    (Alternative m, ReportSchemaErrors m)
    => FromJSON m Token
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
