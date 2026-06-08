module MPFS.Read
    ( factEntriesToJSValue
    , factsResponseToJSValue
    , getTokenFactsFromVerifiedRead
    , verifyFactsResponseWith
    ) where

import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( FactEntry (..)
    , FactsResponse (..)
    , StatusResponse (..)
    )
import Cardano.MPFS.Client.TrustedRoot (TrustedRoot (..))
import Cardano.MPFS.Client.Verify.Replay (VerifyError)
import Cardano.MPFS.Client.Verify.Read
    ( verifyTokenFacts
    , verifiedTokenFacts
    )
import Core.Types.Basic (TokenId)
import Core.Types.Fact (Slot (..))
import Data.ByteString.Char8 qualified as B8
import Data.Functor.Identity (Identity (runIdentity))
import Lib.JSON.Canonical.Extra (object, (.=))
import MPFS.Cage (liftEitherClientM)
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue)

getTokenFactsFromVerifiedRead
    :: ClientM StatusResponse
    -> (TokenId -> ClientM FactsResponse)
    -> TokenId
    -> ClientM JSValue
getTokenFactsFromVerifiedRead getStatus getFacts tokenId = do
    StatusResponse{currentUtxoRoot} <- getStatus
    trustedRoot <-
        liftEitherClientM $ maybeToEither noUtxoRoot currentUtxoRoot
    facts <- getFacts tokenId
    verified <-
        liftEitherClientM
            $ verifyFactsResponseWith
                verifyFactsResponse
                (TrustedRoot trustedRoot)
                facts
    pure verified

verifyFactsResponseWith
    :: Show e
    => (TrustedRoot -> FactsResponse -> Either e FactsResponse)
    -> TrustedRoot
    -> FactsResponse
    -> Either String JSValue
verifyFactsResponseWith verify trustedRoot facts =
    factsResponseToJSValue <$> firstShow (verify trustedRoot facts)

factsResponseToJSValue :: FactsResponse -> JSValue
factsResponseToJSValue FactsResponse{frsFacts} =
    factEntriesToJSValue frsFacts

factEntriesToJSValue :: [FactEntry] -> JSValue
factEntriesToJSValue facts =
    runIdentity $ object =<< traverse factEntryToPair facts

factEntryToPair :: FactEntry -> Identity (String, Identity JSValue)
factEntryToPair FactEntry{feKey = Hex key, feValue = Hex value} =
    pure
        ( B8.unpack key
        , object
            [ "value" .= B8.unpack value
            , "slot" .= Slot 0
            ]
        )

firstShow :: Show e => Either e a -> Either String a
firstShow =
    either (Left . show) Right

verifyFactsResponse
    :: TrustedRoot -> FactsResponse -> Either VerifyError FactsResponse
verifyFactsResponse trustedRoot facts =
    verifiedTokenFacts <$> verifyTokenFacts trustedRoot facts

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e =
    maybe (Left e) Right

noUtxoRoot :: String
noUtxoRoot =
    "GET /status returned no utxo_root; MPFS indexer is not ready"
