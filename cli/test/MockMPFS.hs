module MockMPFS (mockMPFS, withRequestInsert, withFacts, withRequests)
where

import Core.Types.Basic
    ( Address
    , Owner (..)
    , RequestRefId (..)
    , TokenId
    )
import Core.Types.Fact (JSFact, renderFacts)
import Core.Types.Tx
    ( Root (..)
    , UnsignedTx (UnsignedTx)
    , WithUnsignedTx (..)
    )
import Data.Functor.Identity (Identity (..))
import MPFS.API (MPFS (..), RequestInsertBody (..))
import Oracle.Types (RequestZoo, Token (..), TokenState (..))
import Text.JSON.Canonical (JSValue, ToJSON (..))

mockMPFS :: Monad m => MPFS m
mockMPFS =
    MPFS
        { mpfsBootToken = \_ -> error "boot token not implemented"
        , mpfsEndToken = \_ _ -> error "end token not implemented"
        , mpfsRequestInsert = \_ _ RequestInsertBody{value = body} ->
            pure
                $ WithUnsignedTx
                    { unsignedTransaction = UnsignedTx "mock-tx-hash"
                    , value = Just body
                    }
        , mpfsRequestDelete = \_ _ _ -> error "request delete not implemented"
        , mpfsRequestUpdate = \_ _ _ -> error "request update not implemented"
        , mpfsRetractChange = \_ _ -> error "retract change not implemented"
        , mpfsUpdateToken = \_ _ _ -> error "update token not implemented"
        , mpfsGetToken = \_ -> toJSON $ mockToken []
        , mpfsGetTokenFacts = \_ -> toJSON ([] :: [JSFact])
        , mpfsSubmitTransaction = \_ -> error "submit transaction not implemented"
        , mpfsWaitNBlocks = \_ -> error "wait n blocks not implemented"
        , mpfsGetTransaction = \_ -> error "get transaction not implemented"
        }

withRequestInsert
    :: ( Address
         -> TokenId
         -> RequestInsertBody
         -> Identity (WithUnsignedTx JSValue)
       )
    -> MPFS Identity
    -> MPFS Identity
withRequestInsert f mpfs = mpfs{mpfsRequestInsert = f}

withFacts :: Monad m => [JSFact] -> MPFS m -> MPFS m
withFacts fs mpfs = mpfs{mpfsGetTokenFacts = \_ -> pure $ renderFacts fs}

withRequests :: Monad m => [RequestZoo] -> MPFS m -> MPFS m
withRequests reqs mpfs = mpfs{mpfsGetToken = \_ -> toJSON (mockToken reqs)}

mockToken :: [RequestZoo] -> Token Identity
mockToken reqs =
    Token
        { tokenRequests = Identity <$> reqs
        , tokenState =
            TokenState
                { tokenRoot = Root "mock-root"
                , tokenOwner = Owner "mock-owner"
                }
        , tokenRefId = RequestRefId "mock-token-ref-id"
        }
