module AdversarySpec where

import Adversary
import Adversary.ChainSync (Limit (..))
import Data.Aeson (decode, encode)
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import System.Random (mkStdGen)
import Test.Hspec (Spec, it, shouldBe, shouldContain, shouldNotBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, forAll)

spec :: Spec
spec = do
    it "Display Message as String"
        $ toString (Startup{arguments = ["Foo"]})
        `shouldBe` "{\"arguments\":[\"Foo\"],\"tag\":\"Startup\"}"

    it "Reads Limit"
        $ read "20"
        `shouldBe` Limit 20

    prop "Roundtrip messages to/from JSON" prop_roundTrip

    it "readChainPoint can read point"
        $ readChainPoint
            "74b9b4c63f1af41cd51d74d950cc323a9c159eb76fe52cbd8272dde041c4bdbe@40"
        `shouldNotBe` Nothing
    it "readChainPoint can read \"origin\""
        $ readChainPoint
            "origin"
        `shouldBe` Just originPoint

    it "Point aeson instances roundtrips" $ do
        let str =
                "74b9b4c63f1af41cd51d74d950cc323a9c159eb76fe52cbd8272dde041c4bdbe@40"
        let p = fromMaybe (error "failed reading point") $ readChainPoint str
        Aeson.decode (Aeson.encode p) `shouldBe` Just p

    it "Selects a point from a file of points" $ do
        let seed = mkStdGen 0
        lines sampleFile `shouldContain` [selectPointFromFile seed sampleFile]

    it "different seeds can yield different selections" $ do
        let seed = mkStdGen 0
        let seed' = mkStdGen 5
        let p = selectPointFromFile seed sampleFile
        let p' = selectPointFromFile seed' sampleFile

        p `shouldNotBe` p' -- for some seeds

        -- goldens:
        (p, p')
            `shouldBe` ( "8b5479c8fde9927dff66ba22242b76f821b8c6020fbf673e1afd94118c12fa59@172"
                       , "65ddb4e6d384fdf97afee9bee47785192cf0aa040764cb4f794d2a57255eb626@100"
                       )

sampleFile :: String
sampleFile =
    unlines
        [ "da6825fc55326d3ca46846b78575d01b711596f280cb111aee9038e7185bd9f0@31"
        , "da6825fc55326d3ca46846b78575d01b711596f280cb111aee9038e7185bd9f0@31"
        , "da6825fc55326d3ca46846b78575d01b711596f280cb111aee9038e7185bd9f0@31"
        , "da6825fc55326d3ca46846b78575d01b711596f280cb111aee9038e7185bd9f0@31"
        , "da6825fc55326d3ca46846b78575d01b711596f280cb111aee9038e7185bd9f0@31"
        , "d59a1df2b509316a85029ed8493d18aac497624d6a8e4f4e9044d21a3f00c10a@95"
        , "d59a1df2b509316a85029ed8493d18aac497624d6a8e4f4e9044d21a3f00c10a@95"
        , "d59a1df2b509316a85029ed8493d18aac497624d6a8e4f4e9044d21a3f00c10a@95"
        , "d59a1df2b509316a85029ed8493d18aac497624d6a8e4f4e9044d21a3f00c10a@95"
        , "d59a1df2b509316a85029ed8493d18aac497624d6a8e4f4e9044d21a3f00c10a@95"
        , "65ddb4e6d384fdf97afee9bee47785192cf0aa040764cb4f794d2a57255eb626@100"
        , "65ddb4e6d384fdf97afee9bee47785192cf0aa040764cb4f794d2a57255eb626@100"
        , "65ddb4e6d384fdf97afee9bee47785192cf0aa040764cb4f794d2a57255eb626@100"
        , "65ddb4e6d384fdf97afee9bee47785192cf0aa040764cb4f794d2a57255eb626@100"
        , "65ddb4e6d384fdf97afee9bee47785192cf0aa040764cb4f794d2a57255eb626@100"
        , "6f75bf2de93bba19c7c5092e7a904a9a539461fc5ff99e1556d6ab178e048044@160"
        , "6f75bf2de93bba19c7c5092e7a904a9a539461fc5ff99e1556d6ab178e048044@160"
        , "6f75bf2de93bba19c7c5092e7a904a9a539461fc5ff99e1556d6ab178e048044@160"
        , "6f75bf2de93bba19c7c5092e7a904a9a539461fc5ff99e1556d6ab178e048044@160"
        , "6f75bf2de93bba19c7c5092e7a904a9a539461fc5ff99e1556d6ab178e048044@160"
        , "8b5479c8fde9927dff66ba22242b76f821b8c6020fbf673e1afd94118c12fa59@172"
        , "8b5479c8fde9927dff66ba22242b76f821b8c6020fbf673e1afd94118c12fa59@172"
        , "8b5479c8fde9927dff66ba22242b76f821b8c6020fbf673e1afd94118c12fa59@172"
        , "8b5479c8fde9927dff66ba22242b76f821b8c6020fbf673e1afd94118c12fa59@172"
        , "8b5479c8fde9927dff66ba22242b76f821b8c6020fbf673e1afd94118c12fa59@172"
        , "5a47462e396cd20a2c5d15f5eed40b613a38905e6049eaa0b9d3535ded58f44f@224"
        ]
prop_roundTrip :: Property
prop_roundTrip = forAll genMessage $ \msg ->
    let encoded = encode msg
        decoded = decode encoded
    in  decoded == Just msg

genMessage :: Gen Message
genMessage = Startup <$> arbitrary
