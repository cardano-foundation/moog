module Lib.GitHub
    ( GithubResponseError (..)
    , GetGithubFileFailure (..)
    , GithubResponseStatusCodeError (..)
    , githubCommitExists
    , githubDirectoryExists
    , githubUserPublicKeys
    , githubGetFile
    , githubGetCodeOwnersFile
    , githubRepositoryExists

      -- * Utilities for working with GitHub directories
    , githubDownloadDirectory
    , writeToDirectory
    , exitOnException
    , githubStreamDirectoryContents
    ) where

import Control.Exception
    ( Exception
    , SomeException
    )
import Control.Monad.Fix (fix)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , FileName (..)
    , Repository (..)
    , Username (..)
    )
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as B64
import Data.Foldable (Foldable (..), forM_)
import Data.Function ((&))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GitHub (Auth (..), FetchCount (..), github)
import GitHub qualified as GH
import GitHub.Data.Name (Name (..))
import Lib.JSON.Canonical.Extra (object, (.=))
import Network.HTTP.Client
    ( HttpException (..)
    , HttpExceptionContent (StatusCodeException)
    , Response (..)
    )
import Network.HTTP.Types (Status (..))
import Path
    ( Abs
    , Dir
    , File
    , Path
    , Rel
    , SomeBase (..)
    , parent
    , parseAbsDir
    , parseRelDir
    , parseRelFile
    , parseSomeDir
    , stripProperPrefix
    , toFilePath
    , (</>)
    )
import Streaming
    ( MonadIO (liftIO)
    , MonadTrans (lift)
    , Of
    , Stream
    , effect
    )
import Streaming.Prelude (yield)
import Streaming.Prelude qualified as S
import System.Directory
    ( createDirectoryIfMissing
    , getCurrentDirectory
    )
import Text.JSON.Canonical (ToJSON (..))

data GithubResponseError
    = GithubResponseErrorRepositoryNotFound
    | GithubResponseErrorSSHPublicKeysCannotBeFetched String
    | GithubResponseCodeError GithubResponseStatusCodeError
    deriving (Eq, Show)

instance Exception GithubResponseError

instance Monad m => ToJSON m GithubResponseError where
    toJSON GithubResponseErrorRepositoryNotFound =
        object ["error" .= ("repository not found" :: String)]
    toJSON (GithubResponseErrorSSHPublicKeysCannotBeFetched err) =
        object ["error" .= ("SSH public keys cannot be fetched: " ++ err)]
    toJSON (GithubResponseCodeError err) =
        object ["error" .= ("GitHub response code error: " ++ show err)]

data GithubResponseStatusCodeError
    = GithubResponseStatusCodeNotHandledInClient String
    | GithubResponseStatusCodeNotHTTPError String
    deriving (Eq, Show)

instance Exception GithubResponseStatusCodeError

instance Monad m => ToJSON m GithubResponseStatusCodeError where
    toJSON (GithubResponseStatusCodeNotHandledInClient msg) =
        object
            [ "error"
                .= ("GitHub response status code not handled in client: " ++ msg)
            ]
    toJSON (GithubResponseStatusCodeNotHTTPError msg) =
        object
            ["error" .= ("GitHub response status code not HTTP error: " ++ msg)]

-- | Handle http exceptions from GitHub API calls based on the status code.
onStatusCodeOfException
    :: GH.Error
    -> (Int -> IO (Maybe a))
    -> IO (Either GithubResponseStatusCodeError a)
onStatusCodeOfException e f = case e of
    GH.HTTPError
        ( HttpExceptionRequest
                _
                (StatusCodeException response _)
            ) -> case responseStatus response of
            Status c _ -> do
                r <- f c
                case r of
                    Just a -> return $ Right a
                    Nothing ->
                        return
                            $ Left
                            $ GithubResponseStatusCodeNotHandledInClient
                            $ show c
    _ -> return $ Left $ GithubResponseStatusCodeNotHTTPError $ show e

-- | Check if a commit exists in a GitHub repository.
githubCommitExists
    :: Auth -> Repository -> Commit -> IO (Either GithubResponseError Bool)
githubCommitExists auth (Repository owner repo) (Commit sha) = do
    commit <-
        github auth
            $ GH.commitR
                owner'
                repo'
                sha'
    case commit of
        Left e -> do
            res <- onStatusCodeOfException e $ \c -> do
                case c of
                    404 -> return $ Just $ Left GithubResponseErrorRepositoryNotFound
                    422 -> return $ Just $ Right False
                    _ -> return Nothing
            case res of
                Left err -> return $ Left $ GithubResponseCodeError err
                Right a -> return a
        Right _ -> return $ Right True
  where
    owner' = N $ T.pack owner
    repo' = N $ T.pack repo
    sha' = N $ T.pack sha

githubRepositoryExists
    :: Auth -> Repository -> IO (Either GithubResponseStatusCodeError Bool)
githubRepositoryExists auth (Repository owner repo) = do
    response <- github auth $ GH.repositoryR owner' repo'
    case response of
        Left e -> do
            onStatusCodeOfException e $ \_ -> do
                return $ Just False
        Right _ -> return $ Right True
  where
    owner' = N $ T.pack owner
    repo' = N $ T.pack repo

githubDirectoryExists
    :: Auth
    -> Repository
    -> Commit
    -> Directory
    -> IO (Either GithubResponseStatusCodeError Bool)
githubDirectoryExists auth (Repository owner repo) (Commit sha) (Directory dir) = do
    let path = T.pack dir
    contents <-
        github auth
            $ GH.contentsForR
                owner'
                repo'
                path
                (Just sha')
    case contents of
        Left e -> do
            onStatusCodeOfException e $ \_ -> do
                return $ Just False
        Right _ -> return $ Right True
  where
    owner' = N $ T.pack owner
    repo' = N $ T.pack repo
    sha' = T.pack sha

githubUserPublicKeys
    :: Auth -> Username -> IO (Either GithubResponseError [T.Text])
githubUserPublicKeys auth (Username name) = do
    result <-
        github auth $ GH.publicSSHKeysForR (N $ T.pack name) FetchAll
    case result of
        Left e ->
            pure
                $ Left
                $ GithubResponseErrorSSHPublicKeysCannotBeFetched
                $ show e
        Right r -> pure $ Right $ GH.basicPublicSSHKeyKey <$> toList r

githubGetCodeOwnersFile
    :: Auth -> Repository -> IO (Either GetGithubFileFailure T.Text)
githubGetCodeOwnersFile auth repository =
    githubGetFile auth repository Nothing (FileName "CODEOWNERS")

data GetGithubFileFailure
    = GetGithubFileDirectoryNotFound
    | GetGithubFileNotAFile
    | GetGithubFileUnsupportedEncoding String
    | GetGithubFileOtherFailure FilePath String
    | GetGithubFileCodeError GithubResponseStatusCodeError
    | GithubPathParsingError String
    deriving (Eq, Show)

instance Monad m => ToJSON m GetGithubFileFailure where
    toJSON GetGithubFileDirectoryNotFound =
        object ["error" .= ("directory not found" :: String)]
    toJSON GetGithubFileNotAFile =
        object ["error" .= ("not a file" :: String)]
    toJSON (GetGithubFileUnsupportedEncoding enc) =
        object
            [ "error"
                .= ("unsupported encoding: " ++ enc)
            ]
    toJSON (GetGithubFileOtherFailure filename err) =
        object
            [ "error"
                .= ( "error fetching file "
                        ++ filename
                        ++ ": "
                        ++ err
                   )
            ]
    toJSON (GetGithubFileCodeError err) =
        object ["error" .= ("GitHub response code error: " ++ show err)]
    toJSON (GithubPathParsingError err) =
        object ["error" .= ("path parsing error: " ++ err)]

instance Exception GetGithubFileFailure

githubGetFile
    :: Auth
    -> Repository
    -> Maybe Commit
    -> FileName
    -> IO (Either GetGithubFileFailure T.Text)
githubGetFile auth (Repository owner repo) commitM (FileName filename) = do
    response <-
        github auth
            $ GH.contentsForR
                owner'
                repo'
                (T.pack filename)
                ((\(Commit c) -> T.pack c) <$> commitM)
    case response of
        Left e -> do
            res <- onStatusCodeOfException e $ \c -> do
                case c of
                    404 ->
                        pure
                            . Just
                            . Left
                            $ GetGithubFileDirectoryNotFound
                    _ ->
                        pure
                            . Just
                            . Left
                            . GetGithubFileOtherFailure filename
                            $ show e
            case res of
                Left err -> return $ Left $ GetGithubFileCodeError err
                Right a -> return a
        Right (GH.ContentFile contents) -> do
            let content = GH.contentFileContent contents
            case GH.contentFileEncoding contents of
                "base64" ->
                    pure
                        . Right
                        . T.decodeUtf8
                        . B64.decodeLenient
                        . T.encodeUtf8
                        $ content
                enc ->
                    pure
                        . Left
                        . GetGithubFileUnsupportedEncoding
                        $ T.unpack enc
        Right _ ->
            pure
                . Left
                $ GetGithubFileNotAFile
  where
    owner' = N $ T.pack owner
    repo' = N $ T.pack repo

githubStreamDirectoryContents
    :: Auth
    -> Repository
    -> Maybe Commit
    -> Path Rel Dir
    -> Stream
        (Of (Path Rel File, ByteString))
        (ExceptT GetGithubFileFailure IO)
        ()
githubStreamDirectoryContents
    auth
    (Repository owner repo)
    commitM
    startDir =
        ($ startDir) $ fix $ \go dir -> do
            -- Get the contents of the source directory
            response <-
                liftIO
                    $ github auth
                    $ GH.contentsForR
                        owner'
                        repo'
                        (T.dropEnd 1 $ T.pack $ toFilePath dir)
                        ((\(Commit c) -> T.pack c) <$> commitM)
            case response of
                Left e -> do
                    res <- liftIO $ onStatusCodeOfException e $ \c -> do
                        case c of
                            404 ->
                                pure
                                    . Just
                                    $ GetGithubFileDirectoryNotFound
                            _ ->
                                pure
                                    . Just
                                    . GetGithubFileOtherFailure (toFilePath dir)
                                    $ show e
                    case res of
                        Left err -> lift $ throwE $ GetGithubFileCodeError err
                        Right a -> lift $ throwE a
                Right (GH.ContentFile contents) -> do
                    let content = GH.contentFileContent contents
                    ebytes <- case GH.contentFileEncoding contents of
                        "base64" ->
                            pure
                                . Right
                                . B64.decodeLenient
                                . T.encodeUtf8
                                $ content
                        enc ->
                            pure
                                . Left
                                . GetGithubFileUnsupportedEncoding
                                $ T.unpack enc
                    case ebytes of
                        Left err -> lift $ throwE err
                        Right t -> lift (dirToFile dir) >>= yield . (,t)
                Right (GH.ContentDirectory vis) -> forM_ vis $ \case
                    GH.ContentItem _ctype GH.ContentInfo{contentPath} -> do
                        dir' <-
                            lift
                                $ throwPathParsing
                                $ parseRelDir (T.unpack contentPath)
                        go dir'
      where
        owner' = N $ T.pack owner
        repo' = N $ T.pack repo

dirToFile
    :: Monad m => Path b t -> ExceptT GetGithubFileFailure m (Path Rel File)
dirToFile = throwPathParsing . parseRelFile . tailU . toFilePath
  where
    tailU = T.unpack . T.dropWhileEnd (== '/') . T.dropWhile (== '/') . T.pack

throwPathParsing
    :: (Monad m)
    => Either SomeException a
    -> ExceptT GetGithubFileFailure m a
throwPathParsing f = case f of
    Left err -> throwE . GithubPathParsingError . show $ err
    Right file -> pure file

writeToDirectory
    :: Path Rel Dir
    -> Path Abs Dir
    -> Stream (Of (Path Rel File, ByteString)) IO r
    -> IO r
writeToDirectory srcDir targetDir = S.mapM_ writeFile'
  where
    writeFile' (relPath, content) = do
        unrootedPath <- stripProperPrefix srcDir relPath
        let fullPath = targetDir </> unrootedPath
        createDirectoryIfMissing True (toFilePath $ parent fullPath)
        B.writeFile (toFilePath fullPath) content

exitOnException
    :: Stream (Of a) (ExceptT e IO) r -> Stream (Of a) IO (Either e r)
exitOnException strm = effect $ do
    x <- runExceptT $ S.next strm
    pure $ case x of
        Left e -> pure $ Left e
        Right (Left r) -> pure $ Right r
        Right (Right (a, rest)) -> do
            yield a >> exitOnException rest

absolutizePath :: SomeBase x -> IO (Path Abs x)
absolutizePath (Rel relPath) = do
    currDir <- parseAbsDir =<< getCurrentDirectory
    pure $ currDir </> relPath
absolutizePath (Abs absPath) = pure absPath

githubDownloadDirectory
    :: Auth
    -> Repository
    -> Maybe Commit
    -> Directory
    -- ^ Source directory in the GitHub repository
    -> Directory
    -- ^ Target directory on the local filesystem
    -> IO (Either GetGithubFileFailure ())
githubDownloadDirectory
    auth
    repo
    commitM
    (Directory srcDir)
    (Directory targetDir) = runExceptT $ do
        srcDirPath <- throwPathParsing $ parseRelDir srcDir
        -- Ensure the target directory exists
        targetDirPath <- throwPathParsing $ parseSomeDir targetDir
        targetDirAbs <- lift $ absolutizePath targetDirPath
        ExceptT
            $ githubStreamDirectoryContents auth repo commitM srcDirPath
            & exitOnException
            & writeToDirectory srcDirPath targetDirAbs
