module Oracle.Validate.DownloadAssets
    ( validateDownloadAssets
    , validateAssets
    , DownloadAssetsFailure (..)
    , AssetValidationFailure (..)
    , SourceDirFailure (..)
    )
where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic (Directory (..))
import Core.Types.Fact
    ( Fact (..)
    , keyHash
    )
import Effects
    ( Effects (..)
    , GithubEffects (..)
    , hoistValidation
    )
import Lib.GitHub
    ( GetGithubFileFailure
    , GithubResponseStatusCodeError
    )
import Lib.JSON.Canonical.Extra (object, (.=))
import Oracle.Validate.Types
    ( Validate
    , Validated (..)
    , liftMaybe
    , mapFailure
    , notValidated
    , throwFalse
    , throwJusts
    , throwLeft
    )
import System.Directory
    ( writable
    )
import Text.JSON.Canonical (ToJSON (..))
import User.Agent.Types
    ( TestRunId (..)
    , TestRunMap (..)
    , TestRunStatus (..)
    )
import User.Types
    ( Phase (..)
    , TestRun (..)
    )

data AssetValidationFailure
    = AssetValidationSourceFailure SourceDirFailure
    | DownloadAssetsTargetDirNotFound
    | DownloadAssetsTargetDirNotWritable
    | DownloadAssetsGithubError GetGithubFileFailure
    | DownloadAssetsDockerComposeConfigurationFailure String
    | DownloadAssetsDockerComposeFileMissing
    deriving (Show, Eq)

instance Monad m => ToJSON m AssetValidationFailure where
    toJSON (AssetValidationSourceFailure failure) =
        object
            ["error" .= ("Source directory validation failed: " <> show failure)]
    toJSON DownloadAssetsTargetDirNotFound =
        object ["error" .= ("There is no target local directory" :: String)]
    toJSON DownloadAssetsTargetDirNotWritable =
        object
            ["error" .= ("The target local directory is not writable" :: String)]
    toJSON (DownloadAssetsGithubError failure) =
        object
            [ "error"
                .= ( "GitHub error when downloading directory: "
                        <> show failure
                   )
            ]
    toJSON (DownloadAssetsDockerComposeConfigurationFailure failure) =
        object
            [ "error"
                .= ( "Docker Compose configuration error: "
                        <> failure
                   )
            ]
    toJSON DownloadAssetsDockerComposeFileMissing =
        object
            [ "error"
                .= ("docker-compose.yml file is missing in the assets" :: String)
            ]

data DownloadAssetsFailure
    = DownloadAssetsTestRunIdNotFound TestRunId
    | DownloadAssetsValidationError AssetValidationFailure
    deriving (Show, Eq)

instance Monad m => ToJSON m DownloadAssetsFailure where
    toJSON (DownloadAssetsTestRunIdNotFound (TestRunId testid)) =
        object
            [ "error"
                .= ( "Requested test id : "
                        ++ show testid
                        ++ " not found. Please refer to created ones using 'moog agent query'"
                   )
            ]
    toJSON (DownloadAssetsValidationError failure) =
        object ["error" .= failure]

data SourceDirFailure
    = SourceDirFailureDirAbsent
    | SourceDirFailureGithubError GithubResponseStatusCodeError
    deriving (Show, Eq)

checkSourceDirectory
    :: Monad m
    => Effects m
    -> TestRun
    -> m (Maybe SourceDirFailure)
checkSourceDirectory
    Effects
        { githubEffects = GithubEffects{githubDirectoryExists}
        }
    testRun = do
        existsE <-
            githubDirectoryExists
                (repository testRun)
                (commitId testRun)
                (directory testRun)
        pure $ case existsE of
            Left err -> Just $ SourceDirFailureGithubError err
            Right exists ->
                if exists
                    then Nothing
                    else Just SourceDirFailureDirAbsent

downloadAssetsDirectory
    :: Monad m
    => Effects m
    -> TestRun
    -> Directory
    -> m (Maybe GetGithubFileFailure)
downloadAssetsDirectory validation testRun dir = do
    let sourceDir = directory testRun
        commit = commitId testRun
        repository' = repository testRun
    r <-
        (githubDownloadDirectory . githubEffects)
            validation
            repository'
            (Just commit)
            sourceDir
            dir
    pure $ case r of
        Left err -> Just err
        Right () -> Nothing

validateDownloadAssets
    :: Monad m
    => Effects m
    -> TestRunMap
    -> TestRunId
    -> Directory
    -> Validate DownloadAssetsFailure m Validated
validateDownloadAssets validation TestRunMap{pending, running, done} testid dir = do
    pendingR <- filterM inspectTestRunPending pending
    runningR <- filterM inspectTestRunRunning running
    doneR <- filterM inspectTestRunDone done
    let matched =
            (extractTestRunPending <$> pendingR)
                ++ (extractTestRunRunning <$> runningR)
                ++ (extractTestRunDone <$> doneR)
    case matched of
        [] -> notValidated $ DownloadAssetsTestRunIdNotFound testid
        firstMatched : _ ->
            mapFailure DownloadAssetsValidationError
                $ validateAssets dir validation firstMatched
  where
    checkFact testrun = do
        keyId <- keyHash @_ @TestRun testrun
        pure $ testid == TestRunId keyId
    extractTestRunPending :: TestRunStatus PendingT -> TestRun
    extractTestRunPending (StatusPending fact) = factKey fact
    extractTestRunRunning :: TestRunStatus RunningT -> TestRun
    extractTestRunRunning (StatusRunning fact) = factKey fact
    extractTestRunDone :: TestRunStatus DoneT -> TestRun
    extractTestRunDone (StatusDone fact) = factKey fact
    inspectTestRunPending (StatusPending fact) = checkFact $ factKey fact
    inspectTestRunRunning (StatusRunning fact) = checkFact $ factKey fact
    inspectTestRunDone (StatusDone fact) = checkFact $ factKey fact

validateAssets
    :: Monad m
    => Directory
    -> Effects m
    -> TestRun
    -> Validate AssetValidationFailure m Validated
validateAssets
    dir@(Directory directoryPath)
    validation@Effects{fileExists, dockerComposeConfigure}
    testRun = do
        sourceDirValidation <-
            lift $ checkSourceDirectory validation testRun
        _ <-
            mapFailure AssetValidationSourceFailure
                $ throwJusts sourceDirValidation
        targetDirValidation <-
            directoryExists (hoistValidation validation) dir
        permissions <-
            liftMaybe DownloadAssetsTargetDirNotFound targetDirValidation
        Validated <-
            throwFalse (writable permissions) DownloadAssetsTargetDirNotWritable
        downloadTry <- lift $ downloadAssetsDirectory validation testRun dir
        dockerComposeFileExists <- do
            yml <- lift $ fileExists (directoryPath <> "/docker-compose.yml")
            yaml <- lift $ fileExists (directoryPath <> "/docker-compose.yaml")
            pure $ yml <|> yaml
        _ <-
            liftMaybe
                DownloadAssetsDockerComposeFileMissing
                dockerComposeFileExists
        config <- lift $ dockerComposeConfigure dir
        throwLeft DownloadAssetsDockerComposeConfigurationFailure config
        mapFailure DownloadAssetsGithubError $ throwJusts downloadTry
