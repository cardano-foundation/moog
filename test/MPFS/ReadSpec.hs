module MPFS.ReadSpec (spec) where

import Cardano.Ledger.Address (Addr, decodeAddr)
import Cardano.Ledger.Api (ConwayEra, eraProtVerLow)
import Cardano.Ledger.Api.Scripts.Data
    ( Data (..)
    , Datum (..)
    , dataToBinaryData
    )
import Cardano.Ledger.Api.Tx.Out (TxOut, datumTxOutL, mkBasicTxOut)
import Cardano.Ledger.BaseTypes (Inject (inject))
import Cardano.Ledger.Binary.Encoding qualified as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Cardano.MPFS.API.Encoding (Hex (..))
import Cardano.MPFS.API.Types
    ( FactEntry (..)
    , FactsResponse (..)
    , RequestJSON (..)
    , RequestsResponse (..)
    , TokenResponse (..)
    , TokenStateJSON (..)
    , TxInJSON (..)
    , WitnessedRequest (..)
    , WitnessedTokenState (..)
    , WitnessedUtxo (..)
    )
import Cardano.MPFS.API.Types.Common
    ( ChainPointJSON (..)
    , TokenIdJSON (..)
    , UtxoSetWitness (..)
    , VerificationSnapshot (..)
    )
import Core.Types.Change qualified as Change
import Core.Types.Operation qualified as Operation
import Cardano.MPFS.Cage.Types
    ( CageDatum (RequestDatum)
    , OnChainOperation (OpInsert)
    , OnChainRequest (..)
    , OnChainTokenId (..)
    )
import Cardano.MPFS.Client.TrustedRoot (TrustedRoot (..))
import Control.Lens ((&), (.~))
import Core.Types.Basic (Owner (..), RequestRefId (..))
import Core.Types.Fact (Fact (..), Slot (..), parseFacts)
import Core.Types.Tx (Root (..))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import MPFS.Read
    ( factEntriesToJSValue
    , verifyFactsResponseWith
    , verifyTokenResponsesWith
    )
import Oracle.Types
    ( Request (..)
    , RequestZoo (..)
    , Token (..)
    , TokenState (..)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import PlutusTx.Builtins (toBuiltin)
import PlutusTx.IsData.Class (toData)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , renderCanonicalJSON
    , toJSString
    )

spec :: Spec
spec =
    describe "verified MPFS facts reads" $ do
        it "maps verified FactEntry values to moog facts JSON" $ do
            parseFacts (factEntriesToJSValue sampleEntries)
                `shouldBe`
                [ Fact sampleKey sampleValue (Slot 0)
                ]

        it "fails closed when verification rejects the response" $ do
            let result =
                    verifyFactsResponseWith
                        (\_ _ -> Left SampleVerifyError)
                        sampleTrustedRoot
                        unusedFactsResponse

            result `shouldSatisfy` isLeft

        it "maps verified token state and requests to moog Token JSON" $ do
            let result =
                    verifyTokenResponsesWith
                        acceptTokenResponse
                        acceptRequestsResponse
                        sampleTokenId
                        sampleTrustedRoot
                        sampleTokenResponse
                        sampleRequestsResponse

            (result >>= parseToken) `shouldBe` Right sampleToken

        it "fails closed when state or request verification rejects" $ do
            let stateFailure =
                    verifyTokenResponsesWith
                        rejectTokenResponse
                        acceptRequestsResponse
                        sampleTokenId
                        sampleTrustedRoot
                        sampleTokenResponse
                        sampleRequestsResponse
                requestFailure =
                    verifyTokenResponsesWith
                        acceptTokenResponse
                        rejectRequestsResponse
                        sampleTokenId
                        sampleTrustedRoot
                        sampleTokenResponse
                        sampleRequestsResponse

            stateFailure `shouldSatisfy` isLeft
            requestFailure `shouldSatisfy` isLeft

sampleEntries :: [FactEntry]
sampleEntries =
    [ FactEntry
        { feKey = Hex $ canonicalBytes sampleKey
        , feValue = Hex $ canonicalBytes sampleValue
        }
    ]

sampleKey :: JSValue
sampleKey =
    JSString $ toJSString "key"

sampleValue :: JSValue
sampleValue =
    JSString $ toJSString "value"

canonicalBytes :: JSValue -> BS.ByteString
canonicalBytes =
    BL.toStrict . renderCanonicalJSON

sampleTrustedRoot :: TrustedRoot
sampleTrustedRoot =
    TrustedRoot $ Hex ""

unusedFactsResponse :: FactsResponse
unusedFactsResponse =
    FactsResponse
        { frsSnapshot = error "unused snapshot"
        , frsState = error "unused state"
        , frsFacts = []
        }

sampleTokenId :: TokenIdJSON
sampleTokenId =
    TokenIdJSON "token-id"

sampleTokenResponse :: TokenResponse
sampleTokenResponse =
    TokenResponse
        { trSnapshot = unusedSnapshot
        , trState =
            WitnessedTokenState
                { wtsUtxo =
                    WitnessedUtxo
                        { wuTxIn = sampleStateTxIn
                        , wuTxOut = Hex ""
                        , wuProof = Hex ""
                        }
                , wtsState =
                    TokenStateJSON
                        { owner = "owner"
                        , root = Hex sampleRoot
                        , tip = 1000000
                        , processTime = 60000
                        , retractTime = 30000
                        }
                }
        }

sampleRequestsResponse :: RequestsResponse
sampleRequestsResponse =
    RequestsResponse
        { rrSnapshot = unusedSnapshot
        , rrRequestSet = unusedRequestSet
        , rrRequests = [sampleWitnessedRequest]
        }

sampleWitnessedRequest :: WitnessedRequest
sampleWitnessedRequest =
    WitnessedRequest
        { wrUtxo =
            WitnessedUtxo
                { wuTxIn = sampleRequestTxIn
                , -- The verified read now decodes the request from this
                  -- inline-datum TxOut, not from the (unverified, lossy)
                  -- 'RequestJSON' projection below.
                  wuTxOut = Hex sampleRequestTxOut
                , wuProof = Hex ""
                }
        , wrRequest =
            RequestJSON
                { rjToken = sampleTokenId
                , rjOwner = "request-owner"
                , rjKey = Hex $ canonicalBytes sampleKey
                , rjOperation = "insert"
                , rjValue = Just $ Hex $ canonicalBytes sampleValue
                , rjFee = 1000000
                , rjSubmittedAt = 42
                }
        }

-- | The request owner is a 28-byte payment key hash; the decoded moog
-- owner is its lowercase hex.
sampleRequestOwner :: BS.ByteString
sampleRequestOwner = BS.replicate 28 0x07

-- | The on-chain request datum that 'sampleRequestTxOut' carries. An
-- insert of @sampleKey@ ↦ @sampleValue@.
sampleOnChainRequest :: OnChainRequest
sampleOnChainRequest =
    OnChainRequest
        { requestToken = OnChainTokenId (toBuiltin (BS.replicate 28 0x01))
        , requestOwner = toBuiltin sampleRequestOwner
        , requestKey = canonicalBytes sampleKey
        , requestValue = OpInsert (canonicalBytes sampleValue)
        , requestFee = 1000000
        , requestSubmittedAt = 42
        }

-- | A real serialized Conway 'TxOut' whose inline datum is the request
-- datum — the reverse of @MPFS.Read.decodeRequestDatum@, so the verified
-- read recovers the typed request (with its real key\/value).
sampleRequestTxOut :: BS.ByteString
sampleRequestTxOut =
    BL.toStrict
        $ Ledger.serialize (eraProtVerLow @ConwayEra)
        $ (mkBasicTxOut sampleAddr (inject (Coin 0)) :: TxOut ConwayEra)
            & datumTxOutL
                .~ Datum
                    ( dataToBinaryData
                        (Data (toData (RequestDatum sampleOnChainRequest)))
                    )

-- | A minimal valid testnet enterprise (payment-key) address, parsed from
-- its raw header + 28-byte key hash, so we needn't hand-build ledger
-- credential\/hash types.
sampleAddr :: Addr
sampleAddr =
    fromMaybe (error "sampleAddr: decodeAddr failed")
        $ decodeAddr (BS.cons 0x60 (BS.replicate 28 0x07))

sampleToken :: Token Identity
sampleToken =
    Token
        { tokenRefId = txInRef sampleStateTxIn
        , tokenState =
            TokenState
                { tokenRoot = Root $ hexString sampleRoot
                , tokenOwner = Owner "owner"
                }
        , tokenRequests =
            [ Identity
                $ UnknownInsertRequest
                $ Request
                    { outputRefId = txInRef sampleRequestTxIn
                    , owner = Owner (hexString sampleRequestOwner)
                    , change =
                        Change.Change
                            (Change.Key sampleKey)
                            (Operation.Insert sampleValue)
                    }
            ]
        }

sampleStateTxIn :: TxInJSON
sampleStateTxIn =
    TxInJSON
        { tjTxId = Hex $ BS.replicate 32 0x01
        , tjTxIx = 0
        }

sampleRequestTxIn :: TxInJSON
sampleRequestTxIn =
    TxInJSON
        { tjTxId = Hex $ BS.replicate 32 0x02
        , tjTxIx = 1
        }

sampleRoot :: BS.ByteString
sampleRoot =
    BS.pack [0x00 .. 0x1f]

parseToken :: JSValue -> Either String (Token Identity)
parseToken value =
    case fromJSON value of
        Just token -> Right token
        Nothing -> Left "Token JSON did not parse"

txInRef :: TxInJSON -> RequestRefId
txInRef TxInJSON{tjTxId = Hex txId, tjTxIx} =
    RequestRefId
        $ T.pack (hexString txId)
            <> "#"
            <> T.pack (show tjTxIx)

hexString :: BS.ByteString -> String
hexString =
    concatMap (flip showHexByte "")
        . BS.unpack
  where
    showHexByte byte =
        showString [hexDigit $ byte `div` 16, hexDigit $ byte `mod` 16]
    hexDigit n
        | n < 10 = toEnum $ fromEnum '0' + fromIntegral n
        | otherwise = toEnum $ fromEnum 'a' + fromIntegral n - 10

unusedSnapshot :: VerificationSnapshot
unusedSnapshot =
    VerificationSnapshot
        { vsUtxoRoot = Hex sampleRoot
        , vsChainPoint =
            ChainPointJSON
                { cpSlot = 42
                , cpBlockId = Hex $ BS.replicate 32 0x03
                }
        }

unusedRequestSet :: UtxoSetWitness
unusedRequestSet =
    UtxoSetWitness
        { uswEntries = []
        , uswCompletenessProof = Hex ""
        }

data SampleVerifyError = SampleVerifyError
    deriving stock (Show)

acceptTokenResponse
    :: TrustedRoot -> TokenResponse -> Either SampleVerifyError TokenResponse
acceptTokenResponse _ =
    Right

rejectTokenResponse
    :: TrustedRoot -> TokenResponse -> Either SampleVerifyError TokenResponse
rejectTokenResponse _ _ =
    Left SampleVerifyError

acceptRequestsResponse
    :: TokenIdJSON
    -> TrustedRoot
    -> RequestsResponse
    -> Either SampleVerifyError RequestsResponse
acceptRequestsResponse _ _ =
    Right

rejectRequestsResponse
    :: TokenIdJSON
    -> TrustedRoot
    -> RequestsResponse
    -> Either SampleVerifyError RequestsResponse
rejectRequestsResponse _ _ _ =
    Left SampleVerifyError

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
