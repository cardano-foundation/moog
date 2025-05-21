{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Anti.Cli
    ( anti
    ) where

import Anti.API
    ( createToken
    , deleteToken
    , getToken
    , requestChange
    , updateToken
    )
import Anti.Types
    ( Command (..)
    , Directory (..)
    , Operation (..)
    , OracleCommand (..)
    , Platform (..)
    , PublicKeyHash (..)
    , Repository (..)
    , Request (..)
    , RequestRefs (..)
    , Role (..)
    , SHA1 (..)
    , TokenId
    , UserCommand (..)
    , Username (..)
    )
import Data.Aeson (Value, encode, object, (.=))
import Data.Binary.Builder (toLazyByteString)
import Data.Text (Text)
import Network.HTTP.Types (encodePathSegmentsRelative)
import Servant.Client (ClientM)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

anti :: Command -> ClientM Value
anti command = do
    case command of
        UserCommand userCommand tk -> case userCommand of
            RegisterPublicKey{platform, username, pubkeyhash} ->
                manageUser tk platform username pubkeyhash Insert
            UnregisterPublicKey{platform, username, pubkeyhash} ->
                manageUser tk platform username pubkeyhash Delete
            RegisterRole{platform, repository, username, role} ->
                manageRole tk platform repository username role Insert
            UnregisterRole{platform, repository, username, role} ->
                manageRole tk platform repository username role Delete
            RequestTest{platform, repository, username, commit, directory} ->
                requestTestCLI tk platform repository username commit directory
        OracleCommand oracleCommand -> case oracleCommand of
            CreateToken -> createToken
            DeleteToken tk -> deleteToken tk
            GetToken tk -> getToken tk
            UpdateToken tk requests -> updateToken tk $ RequestRefs requests

mkKey :: [String] -> String
mkKey =
    BL.unpack
        . toLazyByteString
        . encodePathSegmentsRelative
        . fmap T.pack

manageUser
    :: TokenId
    -> Platform
    -> Username
    -> PublicKeyHash
    -> Operation
    -> ClientM Value
manageUser
    tokenId
    (Platform platform)
    (Username username)
    (PublicKeyHash pubkeyhash)
    operation =
        requestChange tokenId
            $ Request
                { key = mkKey [platform, username, pubkeyhash]
                , value = ""
                , operation
                }

manageRole
    :: TokenId
    -> Platform
    -> Repository
    -> Username
    -> Role
    -> Operation
    -> ClientM Value
manageRole
    tokenId
    (Platform platform)
    (Repository org repo)
    (Username username)
    (Role role)
    operation =
        requestChange tokenId
            $ Request
                { key = mkKey [platform, org, repo, username, role]
                , value = ""
                , operation
                }

requestTestCLI
    :: TokenId
    -> Platform
    -> Repository
    -> Username
    -> SHA1
    -> Directory
    -> ClientM Value
requestTestCLI
    tokenId
    (Platform platform)
    (Repository org repo)
    (Username username)
    (SHA1 sha1)
    (Directory directory) =
        requestChange tokenId
            $ Request
                { key = mkKey [platform, org, repo, username, sha1, directory]
                , value =
                    BL.unpack
                        $ encode
                        $ object
                            [ "state" .= ("pending" :: Text)
                            ]
                , operation = Insert
                }
