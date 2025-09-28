{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Effects.DownloadFile
    ( DownloadedFileFailure (..)
    , inspectDownloadedFileTemplate
    , inspectDownloadedFile
    , renderDownloadedFileFailure
    , analyzeDownloadedFile
    ) where

import Core.Types.Basic (Commit, FileName (..), Repository)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Yaml qualified as Yaml
import GitHub (Auth)
import Lib.GitHub (GetGithubFileFailure, githubGetFile)
import Lib.JSON.Canonical.Extra (object, (.=))
import Text.JSON.Canonical (ToJSON (..))
import Text.MMark qualified as MMark

data DownloadedFileFailure
    = GithubGetFileError GetGithubFileFailure
    | DownloadedFileParseError String
    | DownloadedFileNotSupported
    deriving (Eq, Show)

instance Monad m => ToJSON m DownloadedFileFailure where
    toJSON (GithubGetFileError failure) =
        object ["githubGetFileError" .= show failure]
    toJSON (DownloadedFileParseError failure) =
        object ["downloadedFileParseError" .= failure]
    toJSON DownloadedFileNotSupported =
        object
            [ "downloadedFileNotSupported"
                .= ("Only `md` and `yaml` files are supported" :: Text)
            ]

renderDownloadedFileFailure :: DownloadedFileFailure -> String
renderDownloadedFileFailure = \case
    GithubGetFileError failure ->
        "Error when interacting with github. Details: " <> show failure
    DownloadedFileParseError failure ->
        "The downloaded file seems to have parse error. Details: "
            <> show failure
    DownloadedFileNotSupported ->
        "Only `md` and `yaml` files are currently supported in validation"

analyzeDownloadedFile
    :: FileName
    -> Either GetGithubFileFailure Text
    -> Either DownloadedFileFailure Text
analyzeDownloadedFile (FileName filename) = \case
    Left failure ->
        Left $ GithubGetFileError failure
    Right file ->
        if T.isSuffixOf "md" (T.pack filename)
            then case MMark.parse filename file of
                Left bundle ->
                    Left $ DownloadedFileParseError $ show bundle
                Right _ ->
                    Right file
            else
                if T.isSuffixOf "yaml" (T.pack filename)
                    then case Yaml.decodeAllEither' @Value (T.encodeUtf8 file) of
                        Left parseError ->
                            Left $ DownloadedFileParseError $ show parseError
                        Right _ ->
                            Right file
                    else
                        Left DownloadedFileNotSupported

inspectDownloadedFileTemplate
    :: Repository
    -> Maybe Commit
    -> FileName
    -> ( Repository
         -> Maybe Commit
         -> FileName
         -> IO (Either GetGithubFileFailure Text)
       )
    -> IO (Either DownloadedFileFailure Text)
inspectDownloadedFileTemplate repo commit filename downloadFile = do
    resp <- downloadFile repo commit filename
    pure $ analyzeDownloadedFile filename resp

inspectDownloadedFile
    :: Auth
    -> Repository
    -> Maybe Commit
    -> FileName
    -> IO (Either DownloadedFileFailure Text)
inspectDownloadedFile auth repo commit filename =
    inspectDownloadedFileTemplate
        repo
        commit
        filename
        $ githubGetFile auth
