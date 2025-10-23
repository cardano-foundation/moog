import Data.ByteString.Char8 qualified as BC
import E2ESpec (e2eSpec)
import GitHub (Auth (..))
import System.Environment (lookupEnv)
import Test.Hspec (beforeAll, hspec)

main :: IO ()
main = hspec $ do
    beforeAll getPAT $ do
        e2eSpec

tryGetPAT :: IO (Maybe Auth)
tryGetPAT = fmap (OAuth . BC.pack) <$> lookupEnv "MOOG_GITHUB_PAT"

getPAT :: IO Auth
getPAT = do
    mpat <- tryGetPAT
    case mpat of
        Just pat -> return pat
        Nothing ->
            error
                "Environment variable MOOG_GITHUB_PAT is not set. \
                \ Please set it to some valid GitHub Personal Access Token with \
                \ read access to public repositories."
