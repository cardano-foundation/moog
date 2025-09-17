{-# OPTIONS_GHC -Wno-orphans #-}

module Adversary
    ( adversary
    , Message (..)
    , toString
    , generatePoints
    , readChainPoint
    , unsafeReadChainPoint
    , unsafeParseChainPointSamples
    , originPoint
    , ChainPointSamples (..)
    ) where

import Adversary.ChainSync
    ( HeaderHash
    , Point
    , repeatedClientChainSync
    )
import Control.Arrow (second)
import Data.Aeson (FromJSON, ToJSON, withText)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Short qualified as SBS
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Generics (Generic)
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Point qualified as Point
import System.Random (StdGen, newStdGen, randomR)
import Text.Read (readMaybe)

originPoint :: Point
originPoint = Network.Point Origin

data Message
    = Startup {arguments :: [String]}
    | Completed {results :: [(Point, Either String Point)]}
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToJSON Point where
    toJSON = Aeson.toJSON . showChainPoint

instance FromJSON Point where
    parseJSON = withText "point" $ \t ->
        maybe
            (fail $ "not a point: " <> T.unpack t)
            pure
            (readChainPoint $ T.unpack t)

readOrFail :: Read a => String -> String -> a
readOrFail msg s =
    fromMaybe
        (error (msg <> " failed to read from " <> s))
        (readMaybe s)

adversary :: [String] -> IO Message
adversary
    args@( magicArg : port : limitArg : chainPointsFilePath : nConnectionsArg
                : hosts
            ) = do
        putStrLn $ toString $ Startup args
        let magic = NetworkMagic{unNetworkMagic = readOrFail "magic" magicArg}
        randomGen <- newStdGen
        ChainPointSamples samplePoints <-
            unsafeParseChainPointSamples <$> readFile chainPointsFilePath
        let startPoints = generatePoints randomGen samplePoints
        let (nConnections :: Int) = readOrFail "nConnections" nConnectionsArg
        res <-
            repeatedClientChainSync
                nConnections
                magic
                hosts
                (readOrFail "port" port)
                startPoints
                (readOrFail "limit" limitArg)
        pure
            $ Completed
            $ map (second (either (Left . show) Right)) res
adversary _ =
    error
        "Expected network-magic, port, sync-length, startPoint, number-of-connections and list-of-hosts arguments"

generatePoints :: StdGen -> NonEmpty Point -> NonEmpty Point
generatePoints g points = NE.unfoldr (fmap Just . randomElement points) g
  where
    randomElement :: NonEmpty a -> StdGen -> (a, StdGen)
    randomElement l g' =
        let (randomIndex, g'') = randomR (0, length l - 1) g' -- untested
        in  (l NE.!! randomIndex, g'')

toString :: Message -> String
toString = TL.unpack . TL.decodeUtf8 . Aeson.encode

newtype ChainPointSamples = ChainPointSamples (NonEmpty Point)
    deriving (Eq, Show)

unsafeParseChainPointSamples :: String -> ChainPointSamples
unsafeParseChainPointSamples = fromMaybe (error "invalid chain points") . parseChainPointSamples

parseChainPointSamples :: String -> Maybe ChainPointSamples
parseChainPointSamples =
    fmap (ChainPointSamples . (originPoint NE.:|))
        . mapM readChainPoint
        . lines

unsafeReadChainPoint :: String -> Point
unsafeReadChainPoint = fromMaybe (error "invalid chain point") . readChainPoint

readChainPoint :: String -> Maybe Point
readChainPoint "origin" = Just originPoint
readChainPoint str = case split (== '@') str of
    [blockHashStr, slotNoStr] -> do
        (hash :: HeaderHash) <-
            Consensus.OneEraHash . SBS.toShort
                <$> either
                    (const Nothing)
                    Just
                    ( B16.decode
                        $ T.encodeUtf8
                        $ T.pack blockHashStr
                    )
        slot <- SlotNo <$> readMaybe slotNoStr
        return $ Network.Point $ At $ Point.Block slot hash
    _ -> Nothing
  where
    split f = map T.unpack . T.split f . T.pack

showChainPoint :: Point -> String
showChainPoint (Network.Point Origin) = "origin"
showChainPoint (Network.Point (At (Point.Block (SlotNo slot) hash))) =
    show hash <> "@" <> show slot
