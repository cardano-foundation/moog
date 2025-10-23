module Main (main) where

import App (Result (..))
import App qualified as Moog
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text.Lazy.IO qualified as T
import Lib.JSON.Canonical.Extra (object, (.=))
import Text.JSON.Canonical (renderCanonicalJSON)
import Text.Pretty.Simple (pString)

main :: IO ()
main = do
    clientResult <- Moog.client
    case clientResult of
        Success pretty e -> do
            let value = renderCanonicalJSON e
            if pretty
                then T.putStr $ pString $ BL.unpack value
                else BL.putStrLn value
        Exceptional pretty ex -> do
            output <- object ["exception" .= show ex]
            let value = renderCanonicalJSON output
            if pretty
                then T.putStr $ pString $ BL.unpack value
                else BL.putStrLn value
        Help -> pure ()
