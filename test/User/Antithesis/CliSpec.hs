module User.Antithesis.CliSpec
    ( spec
    )
where

import Cli (Command (..))
import Lib.Box (Box (..))
import Options (Options (..), parseArgs)
import Paths_moog (version)
import System.Environment (withArgs)
import Test.Hspec (Spec, describe, expectationFailure, it)
import User.Antithesis.Cli (AntithesisCommand (..))

spec :: Spec
spec =
    describe "User.Antithesis.Cli" $
        it "parses moog antithesis runs" $ do
            Box (Options _ command) <-
                withArgs ["antithesis", "runs"] $ parseArgs version
            case command of
                AntithesisCommand (Runs Nothing Nothing) -> pure ()
                _ -> expectationFailure "expected AntithesisCommand Runs"
