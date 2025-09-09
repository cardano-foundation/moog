{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Adversary.ChainSync
    ( clientChainSync
    , repeatedClientChainSync
    , Limit (..)
    , Point
    , HeaderHash
    ) where

import Cardano.Chain.Slotting (EpochSlots (EpochSlots))
import Codec.Serialise qualified as CBOR
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (STM, atomically)
    , StrictTVar
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTVar
    )
import Control.Exception (SomeException, try)
import Control.Tracer (Contravariant (contramap), stdoutTracer)
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Proxy (Proxy))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Data.Word (Word32)
import Network.Mux qualified as Mx
import Network.Socket
    ( AddrInfo (..)
    , AddrInfoFlag (AI_PASSIVE)
    , PortNumber
    , SocketType (Stream)
    , defaultHints
    , getAddrInfo
    )
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Block.Abstract
    ( decodeRawHash
    , encodeRawHash
    )
import Ouroboros.Consensus.Byron.Ledger (CodecConfig (..))
import Ouroboros.Consensus.Cardano.Block
    ( CodecConfig (CardanoCodecConfig)
    )
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Cardano.Node
    ( pattern CardanoNodeToNodeVersion2
    )
import Ouroboros.Consensus.Node.Serialisation
    ( decodeNodeToNode
    , encodeNodeToNode
    )
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger
    ( CodecConfig (ShelleyCodecConfig)
    )
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
    ( castPoint
    , decodeTip
    , encodeTip
    )
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Diffusion.Configuration
    ( PeerSharing (PeerSharingDisabled)
    )
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mock.Chain (Chain)
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , MiniProtocolNum (MiniProtocolNum)
    , OuroborosApplication (OuroborosApplication)
    , OuroborosApplicationWithMinimalCtx
    , RunMiniProtocol (InitiatorProtocolOnly)
    , RunMiniProtocolWithMinimalCtx
    , StartOnDemandOrEagerly (StartOnDemand)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.NodeToNode
    ( ControlMessage (..)
    , ControlMessageSTM
    , DiffusionMode (InitiatorOnlyDiffusionMode)
    , NodeToNodeVersion (NodeToNodeV_14)
    , NodeToNodeVersionData (..)
    , nodeToNodeCodecCBORTerm
    )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    )
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake.Codec
    ( cborTermVersionDataCodec
    , noTimeLimitsHandshake
    , nodeToNodeHandshakeCodec
    )
import Ouroboros.Network.Protocol.Handshake.Version
    ( Acceptable (acceptableVersion)
    , Queryable (queryVersion)
    , simpleSingletonVersions
    )
import Ouroboros.Network.Snocket
    ( makeSocketBearer
    , socketSnocket
    )
import Ouroboros.Network.Socket
    ( ConnectToArgs (..)
    , HandshakeCallbacks (HandshakeCallbacks)
    , connectToNode
    , nullNetworkConnectTracers
    )

type Block = Consensus.CardanoBlock Consensus.StandardCrypto

type Header = Consensus.Header Block

type Tip = Network.Tip Block

type Point = Network.Point Header

type HeaderHash = Network.HeaderHash Block

newtype Limit = Limit {limit :: Word32}
    deriving newtype (Show, Read, Eq, Ord)

repeatedClientChainSync
    :: Int
    -> NetworkMagic
    -> [String]
    -> PortNumber
    -> Point
    -> Limit
    -> IO [Either SomeException Point]
repeatedClientChainSync nConns magic peerNames peerPort startingPoint limit =
    mapConcurrently
        (\(_i, peerName) -> clientChainSync magic peerName peerPort startingPoint limit)
        (zip [1 .. nConns] (cycle peerNames))

clientChainSync
    :: NetworkMagic
    -> String
    -> PortNumber
    -> Point
    -> Limit
    -> IO (Either SomeException Point)
clientChainSync magic peerName peerPort startingPoint limit = withIOManager $ \iocp -> do
    chainvar <- newTVarIO (Chain.Genesis :: Chain Header)

    res <-
        -- To gracefully handle the node getting killed it seems we need
        -- the outer 'try', even if connectToNode already returns 'Either
        -- SomeException'.
        try $ do
            AddrInfo{addrAddress} <- resolve
            connectToNode
                (socketSnocket iocp)
                makeSocketBearer
                ConnectToArgs
                    { ctaHandshakeCodec = nodeToNodeHandshakeCodec
                    , ctaHandshakeTimeLimits = noTimeLimitsHandshake
                    , ctaVersionDataCodec = cborTermVersionDataCodec nodeToNodeCodecCBORTerm
                    , ctaConnectTracers = nullNetworkConnectTracers
                    , ctaHandshakeCallbacks =
                        HandshakeCallbacks acceptableVersion queryVersion
                    }
                mempty
                ( simpleSingletonVersions
                    NodeToNodeV_14
                    ( NodeToNodeVersionData
                        { networkMagic = magic
                        , diffusionMode = InitiatorOnlyDiffusionMode
                        , peerSharing = PeerSharingDisabled
                        , query = False
                        }
                    )
                    (\_ -> app chainvar)
                )
                Nothing
                addrAddress
    case res of
        Left e -> return $ Left e
        Right _ -> pure . Chain.headPoint <$> readTVarIO chainvar
  where
    resolve = do
        let hints =
                defaultHints
                    { addrFlags = [AI_PASSIVE]
                    , addrSocketType = Stream
                    }
        NE.head
            <$> getAddrInfo (Just hints) (Just peerName) (Just $ show peerPort)

    app
        :: StrictTVar IO (Chain Header)
        -> OuroborosApplicationWithMinimalCtx
            Mx.InitiatorMode
            addr
            LBS.ByteString
            IO
            ()
            Void
    app chainvar = demoProtocol2
        $ InitiatorProtocolOnly
        $ mkMiniProtocolCbFromPeer
        $ \_ctx ->
            ( contramap show stdoutTracer
            , codecChainSync
            , ChainSync.chainSyncClientPeer (client chainvar)
            )

    client
        :: StrictTVar IO (Chain Header) -> ChainSyncClient Header Point Tip IO ()
    client chainvar =
        chainSyncClient
            chainvar
            startingPoint
            (controlledClient $ terminateAfterCount chainvar)

    terminateAfterCount
        :: StrictTVar IO (Chain Header) -> STM IO ControlMessage
    terminateAfterCount chainvar = do
        chainLength <- getChainLength chainvar
        pure
            $ if chainLength < limit
                then Continue
                else Terminate

    getChainLength :: StrictTVar IO (Chain Header) -> STM IO Limit
    getChainLength chainvar = Limit . fromIntegral . Chain.length <$> readTVar chainvar

-- TODO: provide sensible limits
-- https://github.com/intersectmbo/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits
        { maximumIngressQueue = maxBound
        }

--
-- Chain sync demo
--

demoProtocol2
    :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b
    -- ^ chainSync
    -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
demoProtocol2 chainSync =
    OuroborosApplication
        [ MiniProtocol
            { miniProtocolNum = MiniProtocolNum 2
            , miniProtocolStart = StartOnDemand
            , miniProtocolLimits = maximumMiniProtocolLimits
            , miniProtocolRun = chainSync
            }
        ]

data Client header point tip m t = Client
    { onRollBackward
        :: point
        -> tip
        -> m (Either t (Client header point tip m t))
    , onRollForward :: header -> m (Either t (Client header point tip m t))
    , points :: [point] -> m (Either t (Client header point tip m t))
    }

controlledClient
    :: (MonadSTM m)
    => ControlMessageSTM m
    -> Client header point tip m ()
controlledClient controlMessageSTM = client
  where
    client =
        Client
            { onRollBackward = \_ _ -> do
                ctrl <- atomically controlMessageSTM
                case ctrl of
                    Continue -> pure (Right client)
                    Quiesce ->
                        error
                            "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
                    Terminate -> pure (Left ())
            , onRollForward = \_ -> do
                ctrl <- atomically controlMessageSTM
                case ctrl of
                    Continue -> pure (Right client)
                    Quiesce ->
                        error
                            "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
                    Terminate -> pure (Left ())
            , points = \_ -> pure (Right client)
            }

chainSyncClient
    :: StrictTVar IO (Chain Header)
    -> Point
    -> Client Header Point Tip IO a
    -> ChainSyncClient Header Point Tip IO a
chainSyncClient chainvar startingPoint client =
    ChainSyncClient
        $ either SendMsgDone initialise <$> getChainPoints
  where
    initialise client' =
        SendMsgFindIntersect [startingPoint]
            $
            -- In this consumer example, we do not care about whether the server
            -- found an intersection or not. If not, we'll just sync from genesis.
            --
            -- Alternative policies here include:
            --  iteratively finding the best intersection
            --  rejecting the server if there is no intersection in the last K blocks
            --
            ClientStIntersect
                { recvMsgIntersectFound = \_ _ -> ChainSyncClient (return (requestNext client'))
                , recvMsgIntersectNotFound = \_ -> ChainSyncClient (return (requestNext client'))
                }

    requestNext client' =
        SendMsgRequestNext
            -- We have the opportunity to do something when receiving
            -- MsgAwaitReply. In this example we don't take up that opportunity.
            (pure ())
            (handleNext client')

    handleNext client' =
        ClientStNext
            { recvMsgRollForward = \header _tip -> ChainSyncClient $ do
                rollForward header
                choice <- onRollForward client' header
                pure $ case choice of
                    Left a -> SendMsgDone a
                    Right client'' -> requestNext client''
            , recvMsgRollBackward = \pIntersect tip -> ChainSyncClient $ do
                rollBackward pIntersect
                choice <- onRollBackward client' pIntersect tip
                pure $ case choice of
                    Left a -> SendMsgDone a
                    Right client'' -> requestNext client''
            }

    getChainPoints = do
        choice <- points client []
        pure $ case choice of
            Left a -> Left a
            Right client' -> Right client'

    rollForward b = atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.addBlock b chain
        writeTVar chainvar chain'

    rollBackward p = atomically $ do
        chain <- readTVar chainvar

        -- We will fail to roll back iff `p` doesn't exist in `chain`
        -- This will happen when we're asked to roll back to `startingPoint`,
        -- which we can check for, or any point before, which we can't
        -- check for. Hence we ignore all failures to rollback and replace the
        -- chain with an empty one if we do.
        let !chain' = fromMaybe Chain.Genesis $ Chain.rollback (castPoint p) chain

        writeTVar chainvar chain'

codecChainSync
    :: Codec
        (ChainSync.ChainSync Header Point Tip)
        CBOR.DeserialiseFailure
        IO
        LBS.ByteString
codecChainSync =
    ChainSync.codecChainSync enc dec CBOR.encode CBOR.decode encTip decTip
  where
    --    enc :: SerialiseNodeToNode blk a => a -> Encoding
    enc = encodeNodeToNode @Block ccfg version
    dec = decodeNodeToNode @Block ccfg version

    encTip = encodeTip (encodeRawHash (Proxy @Block))
    decTip = decodeTip (decodeRawHash (Proxy @Block))
    ccfg =
        CardanoCodecConfig
            (ByronCodecConfig $ EpochSlots 42)
            ShelleyCodecConfig
            ShelleyCodecConfig
            ShelleyCodecConfig
            ShelleyCodecConfig
            ShelleyCodecConfig
            ShelleyCodecConfig

    version = CardanoNodeToNodeVersion2 -- @Block
