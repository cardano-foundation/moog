{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Anti.Types
    ( Command (..)
    , Directory (..)
    , Host (..)
    , Options (..)
    , Platform (..)
    , Port (..)
    , PublicKeyHash (..)
    , Repository (Repository)
    , Request (..)
    , Role (..)
    , SHA1 (..)
    , TokenId (..)
    , TxId (..)
    , Username (..)
    , Operation (..)
    ) where

import Data.Aeson
    ( FromJSON (parseJSON)
    , KeyValue ((.=))
    , ToJSON (toJSON)
    , Value (String)
    , object
    , withObject
    , withText
    , (.:)
    )
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

import qualified Data.Text as T

newtype Platform = Platform String
    deriving (Eq, Show)

newtype SHA1 = SHA1 String
    deriving (Eq, Show)

newtype TxId = TxId String
    deriving (Eq, Show)

newtype Username = Username String
    deriving (Eq, Show)

newtype Role = Role String
    deriving (Eq, Show)

newtype PublicKeyHash = PublicKeyHash String
    deriving (Eq, Show)

newtype Directory = Directory String
    deriving (Eq, Show)

newtype TokenId = TokenId String
    deriving (Eq, Show)

instance ToHttpApiData TokenId where
    toUrlPiece (TokenId tokenId) = T.pack tokenId

instance FromHttpApiData TokenId where
    parseUrlPiece tokenId =
        case T.unpack tokenId of
            "" -> Left "TokenId cannot be empty"
            _ -> Right (TokenId (T.unpack tokenId))

data Request = Request
    { key :: [String]
    , value :: String
    , operation :: Operation
    }
    deriving (Eq, Show)

instance ToJSON Request where
    toJSON (Request{key, value, operation}) =
        object
            [ "key" .= key
            , "value" .= value
            , "operation" .= operation
            ]

instance FromJSON Request where
    parseJSON = withObject "Request" $ \v ->
        Request
            <$> v .: "key"
            <*> v .: "value"
            <*> v .: "operation"

data Repository = Repository
    { organization :: String
    , project :: String
    }
    deriving (Eq, Show)

data Operation = Insert | Delete
    deriving (Eq, Show)

instance ToJSON Operation where
    toJSON Insert = String "insert"
    toJSON Delete = String "delete"

instance FromJSON Operation where
    parseJSON = withText "Operation" $ \v ->
        case v of
            "insert" -> pure Insert
            "delete" -> pure Delete
            _ -> fail $ "Invalid operation: " ++ T.unpack v

data Command
    = RequestTest
        { platform :: Platform
        , repository :: Repository
        , commit :: SHA1
        , directory :: Directory
        , username :: Username
        }
    | RegisterPublicKey
        { platform :: Platform
        , username :: Username
        , pubkeyhash :: PublicKeyHash
        }
    | UnregisterPublicKey
        { platform :: Platform
        , username :: Username
        , pubkeyhash :: PublicKeyHash
        }
    | RegisterRole
        { platform :: Platform
        , repository :: Repository
        , role :: Role
        , username :: Username
        }
    | UnregisterRole
        { platform :: Platform
        , repository :: Repository
        , role :: Role
        , username :: Username
        }
    deriving (Eq, Show)

newtype Port = Port Int
    deriving (Eq, Show)

newtype Host = Host String
    deriving (Eq, Show)

data Options = Options
    { tokenId :: TokenId
    , host :: Host
    , port :: Port
    , command :: Command
    }
    deriving (Eq, Show)