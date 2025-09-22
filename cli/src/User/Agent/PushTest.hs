{-# LANGUAGE QuasiQuotes #-}

module User.Agent.PushTest
    ( PushFailure (..)
    , buildConfigImage
    , Registry (..)
    , pushTestToAntithesis
    , collectImagesFromAssets
    , dockerfile
    , pushTestToAntithesisIO
    , PostTestRunRequest (..)
    , Tag (..)
    , AntithesisAuth (..)
    , renderPostToAntithesis
    , renderTestRun
    , SlackWebhook (..)
    , TestRunWithId (..)
    , pushConfigImage
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Core.Context
    ( WithContext
    )
import Core.Types.Basic
    ( Directory (..)
    , Duration (..)
    , TokenId
    )
import Core.Types.Fact (Fact (..))
import Core.Types.Wallet (Wallet (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Functor (($>), (<&>))
import Data.Functor.Identity (Identity (..))
import Data.List (intercalate, nub, sort)
import Data.String.QQ (s)
import Lib.JSON.Canonical.Extra (object, withObject, (.:), (.=))
import Lib.System (runSystemCommand)
import Oracle.Validate.Types
    ( AValidationResult (..)
    , Validate
    , liftMaybe
    , throwLeft
    )
import System.Environment (lookupEnv)
import System.IO.Temp (withSystemTempDirectory)
import Text.JSON.Canonical
    ( FromJSON (..)
    , ReportSchemaErrors
    , ToJSON (..)
    , renderCanonicalJSON
    )
import User.Agent.Lib (resolveTestRunId, testRunDuration, withState)
import User.Agent.Types
    ( TestRunId (..)
    )
import User.Types (TestRun (..))

dockerfile :: String
dockerfile =
    [s|
FROM docker.io/debian:stable-slim AS build

# Set time zone
ENV TZ="UTC"
RUN ln -snf /usr/share/zoneinfo/${TZ} /etc/localtime && \
    echo ${TZ} > /etc/timezone

COPY . /

RUN sed -i 's/${INTERNAL_NETWORK}/false/g' /docker-compose.yaml
    |]

data PostTestRunRequest = PostTestRunRequest
    { description :: String
    , duration :: Float
    , config_image :: String
    , images :: [String]
    , recipients :: [String]
    , source :: String
    , slack :: Maybe String
    }
    deriving (Show, Eq)

-- FIXME: Add nested object and fix keys
instance Aeson.ToJSON PostTestRunRequest where
    toJSON
        PostTestRunRequest
            { description
            , duration
            , config_image
            , images
            , recipients
            , source
            , slack
            } =
            Aeson.object
                [ "params"
                    Aeson..= Aeson.object fields
                ]
          where
            fields =
                [ "antithesis.description" Aeson..= description
                , "antithesis.duration" Aeson..= duration
                , "antithesis.config_image" Aeson..= config_image
                , "antithesis.images" Aeson..= intercalate ";" images
                , "antithesis.report.recipients"
                    Aeson..= intercalate ";" recipients
                , "antithesis.source" Aeson..= source
                ]
                    <> maybe
                        []
                        ( \wh ->
                            ["antithesis.integrations.slack.callback_url" Aeson..= wh]
                        )
                        slack
pushTestToAntithesis
    :: Wallet
    -> Directory
    -> TestRunId
    -> WithContext m (AValidationResult PushFailure ())
pushTestToAntithesis = error "Not implemented yet"

pushTestToAntithesisIO
    :: MonadIO m
    => TokenId
    -> Registry
    -> AntithesisAuth
    -> Directory
    -> TestRunId
    -> Maybe SlackWebhook
    -> Validate PushFailure (WithContext m) ()
pushTestToAntithesisIO
    tk
    registry
    auth
    dir
    testRunId@(TestRunId trId)
    slack = do
        etag <- liftIO $ buildConfigImage registry dir testRunId
        tag <- throwLeft DockerBuildFailure etag
        epush <- liftIO $ pushConfigImage tag
        void $ throwLeft DockerPushFailure epush
        eimages <- liftIO $ collectImagesFromAssets dir
        images <- throwLeft DockerComposeFailure eimages
        (tr, Duration duration) <- getTestRun tk testRunId
        let body =
                PostTestRunRequest
                    { description = renderTestRun testRunId tr
                    , duration = realToFrac duration * 60
                    , config_image = tagString tag
                    , images
                    , recipients = ["antithesis@cardanofoundation.org"]
                    , source = trId
                    , slack = fmap unSlackWebhook slack
                    }
            post = renderPostToAntithesis auth body
        epost <- liftIO $ curl post
        void $ throwLeft PostToAntithesisFailure epost

renderTestRun :: TestRunId -> TestRun -> String
renderTestRun trId tr =
    BL.unpack . renderCanonicalJSON . runIdentity
        $ toJSON
        $ TestRunWithId trId tr

data TestRunWithId = TestRunWithId
    { testRunId :: TestRunId
    , testRun :: TestRun
    }

instance Monad m => ToJSON m TestRunWithId where
    toJSON (TestRunWithId (TestRunId trId) tr) =
        object
            [ "testRunId" .= trId
            , "testRun" .= tr
            ]

instance ReportSchemaErrors m => FromJSON m TestRunWithId where
    fromJSON = withObject "Repository" $ \v -> do
        trId <- v .: "testRunId"
        tr <- v .: "testRun"
        return $ TestRunWithId trId tr

collectImagesFromAssets :: Directory -> IO (Either String [String])
collectImagesFromAssets (Directory dirname) = do
    output <-
        runSystemCommand
            [("INTERNAL_NETWORK", "true")]
            "docker"
            ["compose", "--project-directory", dirname, "config", "--images"]
    let images = nub . sort . filter (not . null) . lines
    return $ output <&> images

getTestRun
    :: Monad m
    => TokenId
    -> TestRunId
    -> Validate
        PushFailure
        (WithContext m)
        (TestRun, Duration)
getTestRun tk testRunId = do
    mts <- lift $ resolveTestRunId tk testRunId
    Fact tr v <- liftMaybe (Couldn'tResolveTestRunId testRunId) mts
    liftMaybe (Couldn'tResolveTestRunId testRunId)
        $ withState (\state -> (tr, testRunDuration state)) v

data AntithesisAuth = AntithesisAuth
    { username :: String
    , password :: String
    }
    deriving (Show, Eq)

renderPostToAntithesis
    :: AntithesisAuth -> PostTestRunRequest -> (String, [String])
renderPostToAntithesis (AntithesisAuth username password) request =
    let curlArgs = (command, args)
        command = "curl"
        args =
            [ "--fail"
            , "-u"
            , username ++ ":" ++ password
            , "-X"
            , "POST"
            , "https://cardano.antithesis.com/api/v1/launch/cardano"
            , "-H"
            , "Content-Type: application/json"
            , "-d"
            , BL.unpack $ Aeson.encode request
            ]
    in  (curlArgs :: (String, [String]))

curl :: (String, [String]) -> IO (Either String String)
curl (command, args) = runSystemCommand [] command args

newtype Tag = Tag {tagString :: String}
    deriving (Show, Eq)

pushConfigImage :: Tag -> IO (Either String String)
pushConfigImage (Tag tag) =
    lookupEnv "DOCKER_CONFIG" >>= \case
        Just dockerConfigPath ->
            runSystemCommand
                [("DOCKER_CONFIG", dockerConfigPath)]
                "docker"
                [ "push"
                , tag
                ]
        Nothing ->
            runSystemCommand
                []
                "docker"
                [ "push"
                , tag
                ]

newtype Registry = Registry {unRegistry :: String}
    deriving (Show, Eq)

buildConfigImage
    :: Registry -> Directory -> TestRunId -> IO (Either String Tag)
buildConfigImage (Registry registry) (Directory context) (TestRunId trId) =
    withSystemTempDirectory
        "anti-cli-test"
        $ \tmpDir -> do
            let dockerfilePath = tmpDir ++ "/Dockerfile"
                imageName = "cardano-anti-cli-config"
                imageTag = take 10 trId
                tag = registry ++ "/" ++ imageName ++ ":" ++ imageTag
            writeFile dockerfilePath dockerfile
            runSystemCommand
                []
                "docker"
                [ "build"
                , "-f"
                , dockerfilePath
                , "-t"
                , tag
                , context
                ]
                <&> ($> Tag tag)

data PushFailure
    = DockerBuildFailure String
    | DockerPushFailure String
    | DockerComposeFailure String
    | Couldn'tResolveTestRunId TestRunId
    | PostToAntithesisFailure String
    deriving (Show, Eq)

instance Monad m => ToJSON m PushFailure where
    toJSON (DockerBuildFailure msg) =
        object
            [ "dockerBuildFailure" .= msg
            ]
    toJSON (DockerPushFailure msg) =
        object
            [ "dockerPushFailure" .= msg
            ]
    toJSON (DockerComposeFailure msg) =
        object
            [ "dockerComposeFailure" .= msg
            ]
    toJSON (Couldn'tResolveTestRunId (TestRunId trId)) =
        object
            [ "couldntResolveTestRunId" .= trId
            ]
    toJSON (PostToAntithesisFailure msg) =
        object
            [ "postToAntithesisFailure" .= msg
            ]

newtype SlackWebhook = SlackWebhook {unSlackWebhook :: String}
    deriving (Show, Eq)
