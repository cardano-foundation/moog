{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module User.TypesSpec
    ( spec
    )
where

import Core.Types
    ( Directory (Directory)
    , Platform (Platform)
    , PublicKeyHash (..)
    , Repository (Repository, organization, project)
    , SHA1 (SHA1)
    , Username (Username)
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.JSON.Canonical
    ( FromJSON (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    )
import User.Types
    ( Duration (Duration)
    , Reason (..)
    , RegisterUserKey (..)
    , TestRun
        ( TestRun
        , commitId
        , directory
        , platform
        , repository
        , requester
        , testRunIndex
        )
    , TestRunState (..)
    , URL (..)
    )

instance ReportSchemaErrors IO where
    expected expct (Just got) =
        fail
            $ "Expected: "
                ++ expct
                ++ ", but got: "
                ++ got
    expected expct Nothing = fail $ "Expected: " ++ expct

roundTrip :: (ToJSON IO a, FromJSON IO a, Show a, Eq a) => a -> IO ()
roundTrip value = do
    encoded <- toJSON value
    decoded <- fromJSON encoded
    decoded `shouldBe` value

spec :: Spec
spec = do
    describe "TestRun" $ do
        it "roundtrips on the JSON instance" $ do
            let testRun =
                    TestRun
                        { platform = Platform "github"
                        , repository =
                            Repository
                                { organization = "user"
                                , project = "repo"
                                }
                        , commitId = SHA1 "abc123"
                        , directory = Directory "src"
                        , requester = Username "tester"
                        , testRunIndex = 1
                        }
            roundTrip testRun

    describe "TestRunState" $ do
        it "roundtrips on the JSON instance" $ do
            let pending = Pending $ Duration 4
            roundTrip pending
            let rejected =
                    Rejected
                        pending
                        [UnacceptableDuration, UnacceptableCommit]
            roundTrip rejected
            let accepted = Accepted pending
            roundTrip accepted
            let finished = Finished accepted (Duration 4) (URL "")
            roundTrip finished

    describe "RegisterUserKey" $ do
        it "roundtrips on the JSON instance" $ do
            let registerPubKey =
                    RegisterUserKey
                        { platform = Platform "github"
                        , username = Username "tester"
                        , pubkeyhash =
                            PublicKeyHash
                                "AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8"
                        }
            roundTrip registerPubKey
