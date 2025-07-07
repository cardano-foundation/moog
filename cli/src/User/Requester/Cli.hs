{-# LANGUAGE DuplicateRecordFields #-}

module User.Requester.Cli
    ( requesterCmd
    , RequesterCommand (..)
    ) where

import Core.Types
    ( TokenId
    , TxHash
    , Wallet (..)
    , WithTxHash (..)
    )
import MPFS.API
    ( RequestDeleteBody (..)
    , RequestInsertBody (..)
    , requestDelete
    , requestInsert
    )
import Servant.Client (ClientM)
import Submitting (Submitting, signAndSubmit)
import Text.JSON.Canonical (ToJSON (..))
import User.Agent.Cli
    ( AgentCommand (..)
    , agentCmd
    )
import User.Types
    ( Duration
    , Phase (PendingT)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState
    )

data RequesterCommand a where
    RegisterUser :: RegisterUserKey -> RequesterCommand TxHash
    UnregisterUser
        :: RegisterUserKey -> RequesterCommand TxHash
    RegisterRole
        :: RegisterRoleKey -> RequesterCommand TxHash
    UnregisterRole
        :: RegisterRoleKey -> RequesterCommand TxHash
    RequestTest
        :: TestRun
        -> Duration
        -> RequesterCommand (WithTxHash (TestRunState PendingT))

deriving instance Show (RequesterCommand a)
deriving instance Eq (RequesterCommand a)

requesterCmd
    :: Submitting
    -> Wallet
    -> TokenId
    -> RequesterCommand a
    -> ClientM a
requesterCmd sbmt wallet tokenId command = do
    case command of
        RegisterUser request ->
            registerUser sbmt wallet tokenId request
        UnregisterUser request ->
            unregisterUser sbmt wallet tokenId request
        RegisterRole request ->
            registerRole sbmt wallet tokenId request
        UnregisterRole request ->
            unregisterRole sbmt wallet tokenId request
        RequestTest testRun duration ->
            agentCmd sbmt wallet tokenId $ Create testRun duration

registerUser
    :: Submitting
    -> Wallet
    -> TokenId
    -> RegisterUserKey
    -> ClientM TxHash
registerUser
    sbmt
    wallet
    tokenId
    request = fmap txHash
        $ signAndSubmit sbmt wallet
        $ \address -> do
            key <- toJSON request
            value <- toJSON ()
            requestInsert address tokenId
                $ RequestInsertBody{key = key, value = value}

unregisterUser
    :: Submitting
    -> Wallet
    -> TokenId
    -> RegisterUserKey
    -> ClientM TxHash
unregisterUser
    sbmt
    wallet
    tokenId
    request = fmap txHash
        $ signAndSubmit sbmt wallet
        $ \address -> do
            key <- toJSON request
            value <- toJSON ()
            requestDelete address tokenId
                $ RequestDeleteBody{key = key, value = value}

registerRole
    :: Submitting
    -> Wallet
    -> TokenId
    -> RegisterRoleKey
    -> ClientM TxHash
registerRole
    sbmt
    wallet
    tokenId
    request = fmap txHash
        $ signAndSubmit sbmt wallet
        $ \address -> do
            key <- toJSON request
            value <- toJSON ()
            requestInsert address tokenId
                $ RequestInsertBody{key = key, value = value}

unregisterRole
    :: Submitting
    -> Wallet
    -> TokenId
    -> RegisterRoleKey
    -> ClientM TxHash
unregisterRole
    sbmt
    wallet
    tokenId
    request = fmap txHash
        $ signAndSubmit sbmt wallet
        $ \address -> do
            key <- toJSON request
            value <- toJSON ()
            requestDelete address tokenId
                $ RequestDeleteBody{key = key, value = value}
