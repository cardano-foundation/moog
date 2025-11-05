import Oracle.Process (oracleProcess, parseArgs)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    args <- parseArgs
    oracleProcess args
