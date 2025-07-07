module Main (main) where

import App qualified as Anti
import Data.ByteString.Lazy.Char8 qualified as BL
import Lib.JSON
import Text.JSON.Canonical (renderCanonicalJSON)

main :: IO ()
main = do
    (_, walletFile, mpsHost, e) <- Anti.client
    case e of
        Left err -> error $ "Error connecting to mpfs server: " ++ show err
        Right result -> do
            output <-
                object
                    [ "walletFile" .= walletFile
                    , "mpfsHost" .= mpsHost
                    , "result" .= result
                    ]
            BL.putStrLn $ renderCanonicalJSON output
