module Lib.System
    ( runSystemCommand
    , runSystemCommandThrows
    ) where

import System.Exit (ExitCode (..))
import System.Process
    ( CreateProcess (env)
    , proc
    , readCreateProcessWithExitCode
    )

runSystemCommand
    :: [(String, String)]
    -- ^ Environment variables
    -> String
    -- ^ Command
    -> [String]
    -- ^ Arguments
    -> IO (Either String String)
    -- ^ Either error message or command output
runSystemCommand envs command args = do
    let createProcess = (proc command args){env = Just envs}
    (exitCode, output, stderr) <-
        readCreateProcessWithExitCode createProcess ""
    case exitCode of
        ExitFailure _ ->
            pure
                $ Left
                $ command
                    ++ " "
                    ++ unwords args
                    ++ " failed: "
                    ++ stderr
        ExitSuccess -> pure $ Right output

runSystemCommandThrows
    :: [(String, String)]
    -- ^ Environment variables
    -> String
    -- ^ Command
    -> [String]
    -- ^ Arguments
    -> IO String
    -- ^ Command output or throws error
runSystemCommandThrows envs command args = do
    result <- runSystemCommand envs command args
    case result of
        Left err -> error err
        Right output -> pure output
