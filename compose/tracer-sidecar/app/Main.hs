{-# LANGUAGE OverloadedStrings #-}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}

module Main where

import Cardano.Antithesis.LogMessage
import Cardano.Antithesis.Sdk
import Cardano.Antithesis.Sidecar

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import Control.Concurrent
    ( forkIO
    , modifyMVar_
    , newMVar
    , threadDelay
    )
import Control.Monad
    ( filterM
    , forM
    , forM_
    , forever
    , unless
    )
import Data.Aeson
    ( ToJSON (toJSON)
    , Value (Null)
    , eitherDecode
    )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , listDirectory
    )
import System.Environment
    ( getArgs
    , getEnv
    )
import System.FilePath
    ( takeExtension
    , (</>)
    )
import System.IO
    ( BufferMode (LineBuffering, NoBuffering)
    , IOMode (ReadMode)
    , hIsEOF
    , hSetBuffering
    , stdout
    , withFile
    )

-- main ------------------------------------------------------------------------

-- | Main: <program> <directory>
--  Processes existing .jsonl files and tails them for new entries.
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "starting tracer-sidecar..."
    args <- getArgs
    dir <- case args of
             [d] -> return d
             _   -> error "Usage: <executable name> <directory>"


    (nPools :: Int) <- read <$> getEnv "POOLS"

    writeSdkJsonl $ sometimesTracesDeclaration "finds all node log files"

    files <- waitFor (nodeLogFiles dir) $ \files -> do
        threadDelay 2000000 -- allow log files to be created
        putStrLn $ unlines $
            [ "Looking for " <> show nPools <> " log files, found "
                <> show (length files) <> ":"
            ] ++ map ("- " <>) files
        let details = toJSON files
        let cond = length files == nPools
        unless cond $
            writeSdkJsonl $ sometimesFailed "finds all node log files" details
        return cond

    writeSdkJsonl $ sometimesTracesReached "finds all node log files"

    putStrLn $ "Observing .json files: " <> show files

    let spec = mkSpec nPools
    mvar <- newMVar =<< initialStateIO spec
    forM_ files $ \file ->
      forkIO $ tailJsonLines file (modifyMVar_ mvar . flip (processMessageIO spec))
    forever $ threadDelay maxBound
  where
    waitFor :: Monad m => m a -> (a -> m Bool) -> m a
    waitFor act cond = do
        a <- act
        c <- cond a
        if c then return a else waitFor act cond

-- utils -----------------------------------------------------------------------

nodeLogFiles :: FilePath -> IO [FilePath]
nodeLogFiles dir = do
  entries <- listDirectory dir
  let paths = map (\node -> dir </> node </> "node.json") entries
  filterM doesFileExist paths

tailJsonLines :: FilePath -> (LogMessage -> IO ()) -> IO ()
tailJsonLines path action = tailLines path $ \bs ->
  case eitherDecode $ BL.fromStrict bs of
      Right msg -> action msg
      Left _e   -> pure () -- putStrLn $ "warning: unrecognized line: " <> B8.unpack bs <> " " <> show e

tailLines :: FilePath -> (B8.ByteString -> IO ()) -> IO ()
tailLines path callback = withFile path ReadMode $ \h -> do
    -- read up to current EOF without closing the handle
    let drain = do
          eof <- hIsEOF h
          unless eof $ B8.hGetLine h >>= callback >> drain
    drain

    -- switch to unbuffered mode and follow new data
    hSetBuffering h NoBuffering
    forever $ do
      eof <- hIsEOF h
      if eof
         then threadDelay 100000
         else B8.hGetLine h >>= callback
