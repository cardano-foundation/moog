module Lib.SSH.KeySpec
    ( spec
    )
where

import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString qualified as B
import Lib.SSH.Private
    ( KeyPair (..)
    , SSHClient (..)
    , Selection (..)
    , WithSelector (..)
    , sign
    , sshKeyPair
    , sshKeySelectors
    )
import Lib.SSH.Public (makeSSHPublicKey)
import Test.Hspec (Spec, beforeAll, describe, it, shouldBe)
import Test.QuickCheck
    ( Testable (property)
    , ioProperty
    , withMaxSuccess
    )

clientWithSelector :: Maybe String -> SSHClient 'WithSelector
clientWithSelector sel =
    SSHClient
        { sshKeySelector = sel
        , sshKeyFile = "test-E2E/fixtures/test_ed25519"
        , sshKeyPassphrase = "pw"
        }

clientWithoutSelector :: SSHClient 'WithoutSelector
clientWithoutSelector =
    SSHClient
        { sshKeySelector = ()
        , sshKeyFile = "test-E2E/fixtures/test_ed25519"
        , sshKeyPassphrase = "pw"
        }

readKey :: Maybe String -> IO KeyPair
readKey user = do
    Just api <- sshKeyPair $ clientWithSelector user
    pure api

readSelectors :: IO [Selection]
readSelectors = sshKeySelectors clientWithoutSelector

spec :: Spec
spec = do
    describe "SSH Key with selector" $ do
        it "should sign and verify a message" $ ioProperty $ do
            k@KeyPair{publicKey} <- readKey $ Just "test_user"
            pure $ property $ withMaxSuccess 10 $ \msgb -> do
                let msg = B.pack msgb
                let signed = sign k msg
                Ed25519.verify publicKey msg signed
        it "should sign and verify a message with the first key" $ ioProperty $ do
            k@KeyPair{publicKey} <- readKey Nothing
            pure $ property $ withMaxSuccess 10 $ \msgb -> do
                let msg = B.pack msgb
                let signed = sign k msg
                Ed25519.verify publicKey msg signed
    describe "SSH Key without selector" $ beforeAll readSelectors $ do
        it "should read all selectors" $ \sels -> do
            sels
                `shouldBe` [ Selection
                                { selectorName = "test_user"
                                , selectorKey =
                                    makeSSHPublicKey
                                        "AAAAC3NzaC1lZDI1NTE5AAAAIIITaA+1gRPR3BMWGwF5ppDvQDyjqJ1VJNCQTlErA9ot"
                                }
                           ]
