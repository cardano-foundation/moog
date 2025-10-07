module Wallet.CliSpec
    ( spec
    )
where

import Data.Either (isRight)
import Data.Text (Text)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldSatisfy)
import Wallet.Cli
    ( WalletCommand (Create)
    , WalletError (WalletPresent)
    , walletCmd
    )

spec :: Spec
spec = describe "Wallet create" $ do
    it "fails if the wallet is already present" $ do
        withSystemTempDirectory "wallet-cli-spec" $ \dir -> do
            let wallet = dir <> "/wallet"
            writeFile wallet ""
            let command = Create wallet (Nothing :: Maybe Text)
            walletCmd command `shouldReturn` Left WalletPresent
    it "succeeds if the wallet is not present" $ do
        withSystemTempDirectory "wallet-cli-spec" $ \dir -> do
            let wallet = dir <> "/wallet"
            let command = Create wallet (Nothing :: Maybe Text)
            res <- walletCmd command
            res `shouldSatisfy` isRight
