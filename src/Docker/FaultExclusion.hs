module Docker.FaultExclusion
    ( FaultClass (..)
    , FaultExclusions (..)
    , emptyFaultExclusions
    , classifyServices
    , parseFaultExclusions
    )
where

import Control.Exception (IOException, try)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (find)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Data.Yaml qualified as Yaml

data FaultClass = Network | Kill | Pause | Stop
    deriving (Eq, Ord, Show)

data FaultExclusions = FaultExclusions
    { networkExclusion :: [String]
    , killExclusion :: [String]
    , pauseExclusion :: [String]
    , stopExclusion :: [String]
    }
    deriving (Eq, Show)

emptyFaultExclusions :: FaultExclusions
emptyFaultExclusions =
    FaultExclusions
        { networkExclusion = []
        , killExclusion = []
        , pauseExclusion = []
        , stopExclusion = []
        }

classifyServices :: Aeson.Value -> Either String FaultExclusions
classifyServices value =
    case value of
        Aeson.Object object ->
            case KeyMap.lookup servicesKey object of
                Nothing -> Right emptyFaultExclusions
                Just (Aeson.Object services) ->
                    foldl
                        classifyService
                        (Right emptyFaultExclusions)
                        (KeyMap.toList services)
                Just _ -> Left "services: expected map"
        _ -> Left "compose document: expected map"

parseFaultExclusions :: FilePath -> IO (Either String FaultExclusions)
parseFaultExclusions path = do
    result <- try $ Yaml.decodeFileEither path
    case result of
        Left err -> pure $ Left $ path <> ": " <> show (err :: IOException)
        Right (Left err) ->
            pure $ Left $ path <> ": " <> Yaml.prettyPrintParseException err
        Right (Right value) -> pure $ classifyServices value

classifyService
    :: Either String FaultExclusions
    -> (Aeson.Key, Aeson.Value)
    -> Either String FaultExclusions
classifyService acc (serviceKey, serviceValue) = do
    exclusions <- acc
    case labelValue serviceValue of
        Nothing -> Right exclusions
        Just rawLabel ->
            foldl
                (addService serviceName)
                (Right exclusions)
                (labelTokens rawLabel)
  where
    serviceName = Key.toString serviceKey

labelValue :: Aeson.Value -> Maybe String
labelValue (Aeson.Object service) =
    KeyMap.lookup labelsKey service >>= labelsValue
labelValue _ = Nothing

labelsValue :: Aeson.Value -> Maybe String
labelsValue (Aeson.Object labels) =
    case KeyMap.lookup faultLabelKey labels of
        Just (Aeson.String value) -> Just $ Text.unpack value
        _ -> Nothing
labelsValue (Aeson.Array labels) =
    labelFromList $ Vector.toList labels
labelsValue _ = Nothing

labelFromList :: [Aeson.Value] -> Maybe String
labelFromList labels =
    listLabelValue
        =<< find
            (maybe False isFaultLabel . listLabelText)
            labels

listLabelValue :: Aeson.Value -> Maybe String
listLabelValue label = do
    text <- listLabelText label
    stripPrefix (faultLabelName <> "=") text

listLabelText :: Aeson.Value -> Maybe String
listLabelText (Aeson.String value) = Just $ Text.unpack value
listLabelText _ = Nothing

isFaultLabel :: String -> Bool
isFaultLabel label =
    (faultLabelName <> "=") `isPrefixOf` label

labelTokens :: String -> [String]
labelTokens =
    filter (not . null)
        . fmap trimAscii
        . splitOnComma

splitOnComma :: String -> [String]
splitOnComma "" = [""]
splitOnComma input =
    case break (== ',') input of
        (token, "") -> [token]
        (token, _comma : rest) -> token : splitOnComma rest

trimAscii :: String -> String
trimAscii =
    dropWhileEnd isAsciiSpace . dropWhile isAsciiSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate =
    reverse . dropWhile predicate . reverse

isAsciiSpace :: Char -> Bool
isAsciiSpace char =
    char `elem` [' ', '\t', '\n', '\r', '\f', '\v']

addService
    :: String
    -> Either String FaultExclusions
    -> String
    -> Either String FaultExclusions
addService serviceName acc token = do
    exclusions <- acc
    case parseFaultClass token of
        Nothing ->
            Left $
                serviceName
                    <> ": unknown fault class \""
                    <> token
                    <> "\" (expected one of network, kill, pause, stop)"
        Just Network ->
            Right
                exclusions
                    { networkExclusion =
                        networkExclusion exclusions <> [serviceName]
                    }
        Just Kill ->
            Right
                exclusions
                    { killExclusion =
                        killExclusion exclusions <> [serviceName]
                    }
        Just Pause ->
            Right
                exclusions
                    { pauseExclusion =
                        pauseExclusion exclusions <> [serviceName]
                    }
        Just Stop ->
            Right
                exclusions
                    { stopExclusion =
                        stopExclusion exclusions <> [serviceName]
                    }

parseFaultClass :: String -> Maybe FaultClass
parseFaultClass "network" = Just Network
parseFaultClass "kill" = Just Kill
parseFaultClass "pause" = Just Pause
parseFaultClass "stop" = Just Stop
parseFaultClass _ = Nothing

servicesKey :: Aeson.Key
servicesKey = Key.fromString "services"

labelsKey :: Aeson.Key
labelsKey = Key.fromString "labels"

faultLabelKey :: Aeson.Key
faultLabelKey = Key.fromString faultLabelName

faultLabelName :: String
faultLabelName = "com.antithesis.exclude_from_faults"

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] xs = Just xs
stripPrefix _ [] = Nothing
stripPrefix (x : xs) (y : ys)
    | x == y = stripPrefix xs ys
    | otherwise = Nothing
