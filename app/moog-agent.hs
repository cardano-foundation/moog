import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)
import User.Agent.Process (agentProcess, parseArgs)

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    args <- parseArgs
    agentProcess args
