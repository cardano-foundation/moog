module E2ESpec
    ( e2eSpec
    ) where

import GitHub (Auth)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec
    ( ActionWith
    , SpecWith
    , aroundAllWith
    , describe
    , it
    )

runScenario :: FilePath -> IO ()
runScenario script = do
    let scriptFile = "test-E2E/scenarios/" ++ script
    (code, _stdout, sterr) <- readProcessWithExitCode scriptFile [] ""
    case code of
        ExitSuccess ->
            return ()
        _ ->
            error
                $ "Scenario failed: " ++ scriptFile ++ "\n" ++ sterr ++ "\n" ++ _stdout

setup :: ActionWith () -> ActionWith Auth
setup action _auth = do
    action ()
    return ()

e2eSpec :: SpecWith Auth
e2eSpec = do
    aroundAllWith setup $ do
        describe "End-to-End Tests" $ do
            it "should run the real world scenario (VKey user)" $ do
                runScenario "realWorldVKey.sh"
            it "should run the real world scenario (SSH user)" $ do
                runScenario "realWorldSSH.sh"
            it "should retract a request" $ do
                runScenario "retractions.sh"
            it "should validate a user registration and his role" $ do
                runScenario "validateUserRegAddRole.sh"
