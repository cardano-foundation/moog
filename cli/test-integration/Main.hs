import Data.ByteString.Char8 qualified as BC
import GitHub (Auth (..))
import Lib.GitHubSpec (githubSpec)
import Lib.Github.OracleValidationSpec
    ( existenceSpec
    , roleSpecs
    , userSpec
    )
import MPFS.APISpec (mpfsAPISpec)
import System.Environment (lookupEnv)
import Test.Hspec (beforeAll, hspec)

main :: IO ()
main = hspec $ do
    beforeAll getPAT $ do
        githubSpec
        existenceSpec
        roleSpecs
        mpfsAPISpec
    userSpec

tryGetPAT :: IO (Maybe Auth)
tryGetPAT = fmap (OAuth . BC.pack) <$> lookupEnv "ANTI_GITHUB_PAT"

getPAT :: IO Auth
getPAT = do
    mpat <- tryGetPAT
    case mpat of
        Just pat -> return pat
        Nothing ->
            error
                "Environment variable ANTI_GITHUB_PAT is not set. \
                \ Please set it to some valid GitHub Personal Access Token with \
                \ read access to public repositories."
