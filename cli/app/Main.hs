module Main where

import qualified Pop.Cli as Cli
import System.Environment (getArgs)

main :: IO ()
main = do
    getArgs >>= Cli.pop Cli.conduitRuntime >>= print
