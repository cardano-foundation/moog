{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Core.Types
    ( Owner (..)
    , SignedTx (..)
    , UnsignedTx (..)
    , Wallet (..)
    )
import Core.Types qualified as Types
import Data.Aeson (encode, (.=))
import Data.Aeson.Types (object)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative
    ( Parser
    , command
    , execParser
    , fullDesc
    , help
    , helper
    , hsubparser
    , info
    , metavar
    , optional
    , progDesc
    , strArgument
    , (<**>)
    )
import Submitting (readWallet)
import Wallet.Cli (WalletCommand (Create), walletCmd)

data Commands
    = CreateWallet FilePath
    | Address FilePath
    | SignTransaction FilePath FilePath (Maybe FilePath)

optionCreateWallet :: Parser Commands
optionCreateWallet =
    CreateWallet
        <$> strArgument (metavar "WALLET_FILE" <> help "Path to the wallet file")

optionAddress :: Parser Commands
optionAddress =
    Address
        <$> strArgument
            ( metavar "WALLET_FILE"
                <> help "Path to the wallet file to get the address from"
            )

optionSignTransaction :: Parser Commands
optionSignTransaction =
    SignTransaction
        <$> strArgument
            ( metavar "WALLET_FILE"
                <> help "Path to the wallet file to use for signing"
            )
        <*> strArgument
            ( metavar "UNSIGNED_TRANSACTION_FILE"
                <> help "Path to the transaction file"
            )
        <*> optional
            ( strArgument
                ( metavar "SIGNED_TRANSACTION_FILE"
                    <> help
                        "Path to save the signed transaction file (if different from the unsigned one)"
                )
            )

parseArgs :: Parser Commands
parseArgs =
    hsubparser
        ( command
            "create"
            (info optionCreateWallet (progDesc "Create a new wallet"))
            <> command
                "sign"
                (info optionSignTransaction (progDesc "Sign a transaction"))
            <> command
                "address"
                (info optionAddress (progDesc "Get the address from a wallet file"))
        )

main :: IO ()
main = do
    commands <-
        execParser
            (info (parseArgs <**> helper) (fullDesc <> progDesc "Wallet CLI"))
    case commands of
        CreateWallet walletFile -> createWallet walletFile
        Address walletFile -> addressCmd walletFile
        SignTransaction walletFile unsignedFile signedFile ->
            signTransactionCmd walletFile unsignedFile signedFile

signTransactionCmd :: FilePath -> FilePath -> Maybe FilePath -> IO ()
signTransactionCmd walletFile unsignedFile signedFile = do
    Wallet _ _ sign <- readWallet walletFile
    unsignedTx <- T.strip <$> T.readFile unsignedFile
    case sign $ UnsignedTx unsignedTx of
        Right (SignedTx signedTx) -> do
            let outputFile = fromMaybe unsignedFile signedFile
            T.writeFile outputFile signedTx
        Left e -> putStrLn $ "Error signing transaction: " ++ show e

addressCmd :: FilePath -> IO ()
addressCmd walletFile = do
    Wallet (Types.Address address) (Owner hash) _ <-
        readWallet walletFile
    BL.putStrLn
        $ encode
        $ object
            [ "address" .= address
            , "publicKeyHash" .= hash
            ]

createWallet :: FilePath -> IO ()
createWallet walletFile = void $ walletCmd (Left walletFile) Create
