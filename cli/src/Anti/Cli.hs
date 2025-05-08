{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Anti.Cli (
    anti
) where

import Anti.API (request)
import Anti.Types
    ( Command (..)
    , Directory (..)
    , Operation (..)
    , Platform (..)
    , PublicKeyHash (..)
    , Repository (..)
    , Request (..)
    , Role (..)
    , SHA1 (..)
    , TokenId
    , Username (..)
    )
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import Servant.Client ( ClientM)

anti :: TokenId -> Command -> ClientM Value
anti tk command = do
    case command of
        RegisterPublicKey{platform, username, pubkeyhash} ->
            manageUser tk platform username pubkeyhash Insert
        UnregisterPublicKey{platform, username, pubkeyhash} ->
            manageUser tk platform username pubkeyhash Delete
        RegisterRole{platform, repository, username, role} ->
            manageRole tk platform repository username role Insert
        UnregisterRole{platform, repository, username, role} ->
            manageRole tk platform repository username role Delete
        RequestTest{platform, repository, username, commit, directory} ->
            requestTest tk platform repository username commit directory

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
        request tokenId
            $ Request
                { key = [platform, username, pubkeyhash]
                , value = toJSON ()
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
        request tokenId
            $ Request
                { key = [platform, org, repo, username, role]
                , value = toJSON ()
                , operation
                }

requestTest
    :: TokenId
    -> Platform
    -> Repository
    -> Username
    -> SHA1
    -> Directory
    -> ClientM Value
requestTest
    tokenId
    (Platform platform)
    (Repository org repo)
    (Username username)
    (SHA1 sha1)
    (Directory directory) =
        request tokenId
            $ Request
                { key = [platform, org, repo, username, sha1, directory]
                , value =
                    object
                        [ "state" .= ("pending" :: Text)
                        ]
                , operation = Insert
                }
