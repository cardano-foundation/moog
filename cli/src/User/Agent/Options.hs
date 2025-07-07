{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module User.Agent.Options
    ( agentCommandParser
    ) where

import Core.Options
    ( commitOption
    , directoryOption
    , platformOption
    , repositoryOption
    , usernameOption
    )
import Core.Types (WithTxHash)
import Lib.Box (Box (..))
import Options.Applicative
    ( Alternative (..)
    , Parser
    , auto
    , command
    , eitherReader
    , help
    , hsubparser
    , info
    , long
    , option
    , progDesc
    , str
    )
import User.Agent.Cli (AgentCommand (..), IsReady (NotReady))
import User.Types
    ( Duration (..)
    , Phase (..)
    , Reason (..)
    , TestRun (..)
    , TestRunState
    , URL (..)
    )

agentCommandParser
    :: Parser (Box (AgentCommand NotReady))
agentCommandParser =
    hsubparser
        ( command
            "accept-test"
            ( info
                (Box <$> acceptTestOptions)
                (progDesc "Request a test on a specific platform")
            )
            <> command
                "reject-test"
                ( info
                    (Box <$> rejectTestOptions)
                    (progDesc "Reject a test with a reason")
                )
            <> command
                "report-test"
                ( info
                    (Box <$> reportTestOptions)
                    (progDesc "Report the result of a test run")
                )
        )

acceptTestOptions
    :: Parser (AgentCommand NotReady (WithTxHash (TestRunState RunningT)))
acceptTestOptions =
    fmap (`Accept` ())
        $ TestRun
            <$> platformOption
            <*> repositoryOption
            <*> directoryOption
            <*> commitOption
            <*> pure 1
            <*> usernameOption

reasonParser :: Parser Reason
reasonParser =
    option
        (eitherReader readReason)
        (long "reason" <> help "Reason for rejection")
  where
    readReason :: String -> Either String Reason
    readReason "duration" = pure UnacceptableDuration
    readReason "platform" = pure UnacceptablePlatform
    readReason "repository" = pure UnacceptableRepository
    readReason "commit" = pure UnacceptableCommit
    readReason "round" = pure UnacceptableRound
    readReason _ =
        Left
            "Invalid reason. Valid options are: duration, platform, repository, commit, round."

rejectTestOptions
    :: Parser (AgentCommand NotReady (WithTxHash (TestRunState DoneT)))
rejectTestOptions = do
    testRun <-
        TestRun
            <$> platformOption
            <*> repositoryOption
            <*> directoryOption
            <*> commitOption
            <*> pure 1
            <*> usernameOption
    reason <- many reasonParser
    pure $ Reject testRun () reason

reportTestOptions
    :: Parser (AgentCommand NotReady (WithTxHash (TestRunState DoneT)))
reportTestOptions = do
    testRun <-
        TestRun
            <$> platformOption
            <*> repositoryOption
            <*> directoryOption
            <*> commitOption
            <*> pure 1
            <*> usernameOption
    duration <-
        Duration
            <$> option
                auto
                (long "duration" <> help "Duration of the test run in seconds")
    url <-
        URL <$> option str (long "url" <> help "URL of the test report")
    pure $ Report testRun () duration url
