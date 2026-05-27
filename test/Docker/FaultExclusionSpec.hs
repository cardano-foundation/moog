{-# LANGUAGE QuasiQuotes #-}

module Docker.FaultExclusionSpec
    ( spec
    )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.String.QQ (s)
import Data.Yaml qualified as Yaml
import Docker.FaultExclusion
    ( FaultExclusions (..)
    , classifyServices
    , emptyFaultExclusions
    , parseFaultExclusions
    )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

spec :: Spec
spec =
    describe "Docker.FaultExclusion" $ do
        it "returns empty exclusions for an empty services map" $
            classifyServices
                ( compose
                    [s|
services: {}
                    |]
                )
                `shouldBe` Right emptyFaultExclusions

        it "returns empty exclusions for a service without labels" $
            classifyServices
                ( compose
                    [s|
services:
  tracer:
    image: tracer:latest
                    |]
                )
                `shouldBe` Right emptyFaultExclusions

        it "classifies a map-form network label" $
            classifyServices
                ( compose
                    [s|
services:
  tracer:
    labels:
      com.antithesis.exclude_from_faults: network
                    |]
                )
                `shouldBe` Right
                    emptyFaultExclusions{networkExclusion = ["tracer"]}

        it "classifies a service excluded from all fault classes" $
            classifyServices
                ( compose
                    [s|
services:
  tracer:
    labels:
      com.antithesis.exclude_from_faults: network,kill,pause,stop
                    |]
                )
                `shouldBe` Right
                    FaultExclusions
                        { networkExclusion = ["tracer"]
                        , killExclusion = ["tracer"]
                        , pauseExclusion = ["tracer"]
                        , stopExclusion = ["tracer"]
                        }

        it "keeps service order with disjoint labels" $
            classifyServices
                ( compose
                    [s|
services:
  alpha:
    labels:
      com.antithesis.exclude_from_faults: network
  beta:
    labels:
      com.antithesis.exclude_from_faults: kill,pause
                    |]
                )
                `shouldBe` Right
                    FaultExclusions
                        { networkExclusion = ["alpha"]
                        , killExclusion = ["beta"]
                        , pauseExclusion = ["beta"]
                        , stopExclusion = []
                        }

        it "trims surrounding whitespace from label tokens" $
            classifyServices
                ( compose
                    [s|
services:
  tracer:
    labels:
      com.antithesis.exclude_from_faults: " network , kill "
                    |]
                )
                `shouldBe` Right
                    emptyFaultExclusions
                        { networkExclusion = ["tracer"]
                        , killExclusion = ["tracer"]
                        }

        it "drops empty label tokens" $
            classifyServices
                ( compose
                    [s|
services:
  tracer:
    labels:
      com.antithesis.exclude_from_faults: network,,kill
                    |]
                )
                `shouldBe` Right
                    emptyFaultExclusions
                        { networkExclusion = ["tracer"]
                        , killExclusion = ["tracer"]
                        }

        it "reports the service name and token for an unknown fault class" $
            classifyServices
                ( compose
                    [s|
services:
  tracer:
    labels:
      com.antithesis.exclude_from_faults: netwrk
                    |]
                )
                `shouldSatisfy` containsLeft "tracer" "netwrk"

        it "classifies a list-form label" $
            classifyServices
                ( compose
                    [s|
services:
  tracer:
    labels:
      - com.antithesis.exclude_from_faults=network,kill
                    |]
                )
                `shouldBe` Right
                    emptyFaultExclusions
                        { networkExclusion = ["tracer"]
                        , killExclusion = ["tracer"]
                        }

        it "prefixes parse errors with the input path" $
            withSystemTempDirectory
                "fault-exclusion-spec"
                $ \dir -> do
                    let path = dir </> "missing-compose.yaml"
                    result <- parseFaultExclusions path
                    result `shouldSatisfy` containsLeft path ""

compose :: String -> Aeson.Value
compose yaml =
    case Yaml.decodeEither' $ BS.pack yaml of
        Left err -> error $ Yaml.prettyPrintParseException err
        Right value -> value

containsLeft :: String -> String -> Either String a -> Bool
containsLeft first second value =
    isLeft value
        && either
            (\err -> first `isInfixOf` err && second `isInfixOf` err)
            (const False)
            value
