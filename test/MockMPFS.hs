module MockMPFS (mockMPFS, withFacts, withRequests)
where

import Core.Types.Basic
    ( Owner (..)
    , RequestRefId (..)
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
import Text.JSON.Canonical (ToJSON (..))

mockMPFS :: Monad m => MPFS m
mockMPFS =
    MPFS
        { mpfsBootToken = \_ _ -> error "boot token not implemented"
        , mpfsEndToken = \_ _ -> error "end token not implemented"
        , mpfsRequestInsertFromFacts = \_ _ RequestInsertBody{value = body} ->
            pure
                $ WithUnsignedTx
                    { unsignedTransaction = UnsignedTx "mock-tx-hash"
                    , value = Just body
                    }
        , mpfsRequestDeleteFromFacts =
            \_ _ _ -> error "request delete facts not implemented"
        , mpfsRequestUpdateFromFacts =
            \_ _ _ -> error "request update facts not implemented"
        , mpfsRetractChangeFromFacts =
            \_ _ -> error "retract change facts not implemented"
        , mpfsUpdateTokenFromFacts =
            \_ _ _ -> error "update token facts not implemented"
        , mpfsRejectTokenFromFacts =
            \_ _ _ -> error "reject token facts not implemented"
        , mpfsGetToken = \_ -> toJSON $ mockToken []
        , mpfsGetTokenFacts = \_ -> toJSON ([] :: [JSFact])
        }

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
