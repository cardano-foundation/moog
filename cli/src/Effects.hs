{-# HLINT ignore "Use unless" #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Effects
    ( Effects (..)
    , GithubEffects (..)
    , KeyFailure (..)
    , mkEffects
    , insertValidation
    , deleteValidation
    , updateValidation
    , hoistValidation
    , getFacts
    , getTestRuns
    , getTokenRequests
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control
    ( MonadTransControl (..)
    , control
    , controlT
    )
import Core.Types.Basic
    ( Commit
    , Directory (..)
    , FileName
    , GithubRepository
    , GithubUsername
    , TokenId
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..), JSFact, parseFacts)
import Core.Types.Operation (Op (..))
import Data.Functor (($>), (<&>))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Docker (dockerCompose)
import Effects.DownloadFile
    ( DownloadedFileFailure
    , inspectDownloadedFile
    )
import Effects.RegisterRole
    ( RepositoryRoleFailure
    , inspectRepoRoleForUser
    )
import Effects.RegisterUser
    ( SSHPublicKeyFailure
    , VKey
    , VKeyFailure (..)
    , inspectPublicKey
    , inspectVKey
    )
import GitHub (Auth)
import Lib.GitHub qualified as GitHub
import Lib.JSON.Canonical.Extra (object, (.=))
import Lib.SSH.Private (KeyPair, SSHClient, WithSelector (..))
import Lib.SSH.Private qualified as SSH
import Lib.SSH.Public (SSHPublicKey)
import MPFS.API (MPFS (..))
import Oracle.Types (RequestZoo, Token (tokenRequests))
import Oracle.Validate.Types (Validate, notValidated)
import Servant.Client (ClientM)
import System.Directory (Permissions)
import System.Directory qualified as System
import System.IO.Temp qualified as Temp
import Text.JSON.Canonical (FromJSON (..), ToJSON)
import Text.JSON.Canonical.Class (ToJSON (..))
import User.Types (TestRun)

data GithubEffects m = GithubEffects
    { githubCommitExists
        :: GithubRepository
        -> Commit
        -> m (Either GitHub.GithubResponseError Bool)
    , githubDirectoryExists
        :: GithubRepository
        -> Commit
        -> Directory
        -> m (Either GitHub.GithubResponseStatusCodeError Bool)
    , githubUserPublicKeys
        :: GithubUsername
        -> SSHPublicKey
        -> m (Maybe SSHPublicKeyFailure)
    , githubUserVKeys
        :: GithubUsername
        -> VKey
        -> m (Maybe VKeyFailure)
    , githubRepositoryExists
        :: GithubRepository
        -> m (Either GitHub.GithubResponseStatusCodeError Bool)
    , githubRepositoryRole
        :: GithubUsername
        -> GithubRepository
        -> m (Maybe RepositoryRoleFailure)
    , githubGetFile
        :: GithubRepository
        -> Maybe Commit
        -> FileName
        -> m (Either DownloadedFileFailure Text)
    , githubDownloadDirectory
        :: GithubRepository
        -> Maybe Commit
        -> Directory
        -> Directory
        -> m (Either GitHub.GetGithubFileFailure ())
    }

hoistGithubEffects
    :: (MonadTransControl t, Monad m)
    => GithubEffects m
    -> GithubEffects (t m)
hoistGithubEffects
    GithubEffects
        { githubCommitExists
        , githubDirectoryExists
        , githubUserPublicKeys
        , githubUserVKeys
        , githubRepositoryExists
        , githubRepositoryRole
        , githubGetFile
        , githubDownloadDirectory
        } =
        GithubEffects
            { githubCommitExists =
                \repo commit -> f $ githubCommitExists repo commit
            , githubDirectoryExists =
                \repo commit dir -> f $ githubDirectoryExists repo commit dir
            , githubUserPublicKeys =
                \username publicKey -> f $ githubUserPublicKeys username publicKey
            , githubUserVKeys =
                \username vkey -> f $ githubUserVKeys username vkey
            , githubRepositoryExists = f . githubRepositoryExists
            , githubRepositoryRole =
                \username repository -> f $ githubRepositoryRole username repository
            , githubGetFile =
                \repository commit filename -> f $ githubGetFile repository commit filename
            , githubDownloadDirectory =
                \repository commit sourceDir targetDir ->
                    f
                        $ githubDownloadDirectory repository commit sourceDir targetDir
            }
      where
        f = lift

-- | Abstract the side effects necessary for validation.
data Effects m = Effects
    { mpfsGetFacts
        :: forall k v
         . (FromJSON Maybe k, FromJSON Maybe v)
        => m [Fact k v]
    , mpfsGetTestRuns :: m [TestRun]
    , mpfsGetTokenRequests :: m [RequestZoo]
    , githubEffects :: GithubEffects m
    , withSystemTempDirectory
        :: forall a
         . String
        -> (FilePath -> m a)
        -> m a
    , writeTextFile :: FilePath -> Text -> m ()
    , withCurrentDirectory :: forall a. FilePath -> m a -> m a
    , directoryExists :: Directory -> m (Maybe Permissions)
    , fileExists :: FilePath -> m (Maybe Permissions)
    , decodePrivateSSHFile :: SSHClient 'WithSelector -> m (Maybe KeyPair)
    , dockerComposeConfigure :: Directory -> m (Either String ())
    }

hoistValidation
    :: (MonadTransControl t, Monad m)
    => Effects m
    -> Effects (t m)
hoistValidation
    Effects
        { mpfsGetFacts
        , mpfsGetTestRuns
        , mpfsGetTokenRequests
        , githubEffects
        , withSystemTempDirectory
        , withCurrentDirectory
        , writeTextFile
        , directoryExists
        , fileExists
        , decodePrivateSSHFile
        , dockerComposeConfigure
        } =
        Effects
            { mpfsGetFacts = f mpfsGetFacts
            , mpfsGetTestRuns = f mpfsGetTestRuns
            , mpfsGetTokenRequests = f mpfsGetTokenRequests
            , githubEffects = hoistGithubEffects githubEffects
            , withSystemTempDirectory =
                \template action -> controlT
                    $ \run -> withSystemTempDirectory template (run . action)
            , withCurrentDirectory = \dir action -> controlT
                $ \run -> withCurrentDirectory dir (run action)
            , writeTextFile = \path content -> f $ writeTextFile path content
            , directoryExists = f . directoryExists
            , fileExists = f . fileExists
            , decodePrivateSSHFile = f . decodePrivateSSHFile
            , dockerComposeConfigure = f . dockerComposeConfigure
            }
      where
        f = lift

getFacts
    :: Applicative m
    => MPFS m
    -> (FromJSON Maybe k, FromJSON Maybe v)
    => Maybe TokenId
    -> m [Fact k v]
getFacts mpfs = maybe (pure []) (fmap parseFacts . mpfsGetTokenFacts mpfs)

getTestRuns :: Applicative m => MPFS m -> Maybe TokenId -> m [TestRun]
getTestRuns mpfs tk =
    mapMaybe (\(Fact k _ _ :: JSFact) -> fromJSON k) <$> getFacts mpfs tk

getTokenRequests
    :: Monad m => MPFS m -> Maybe TokenId -> m [RequestZoo]
getTokenRequests mpfs tk = case tk of
    Nothing -> pure []
    Just tokenId -> do
        mtoken <- fromJSON <$> mpfsGetToken mpfs tokenId
        pure $ maybe [] (fmap runIdentity . tokenRequests) mtoken

mkGithubEffects
    :: Auth
    -> GithubEffects ClientM
mkGithubEffects auth =
    GithubEffects
        { githubCommitExists = \repository commit ->
            liftIO $ GitHub.githubCommitExists auth repository commit
        , githubDirectoryExists = \repository commit dir ->
            liftIO $ GitHub.githubDirectoryExists auth repository commit dir
        , githubUserPublicKeys = \username publicKey ->
            liftIO $ inspectPublicKey auth username publicKey
        , githubUserVKeys = \username vkey ->
            liftIO $ inspectVKey auth username vkey
        , githubRepositoryExists = liftIO . GitHub.githubRepositoryExists auth
        , githubRepositoryRole = \username repository ->
            liftIO $ inspectRepoRoleForUser auth username repository
        , githubGetFile = \repository commit filename ->
            liftIO $ inspectDownloadedFile auth repository commit filename
        , githubDownloadDirectory = \repository commit sourceDir targetDir ->
            liftIO
                $ GitHub.githubDownloadDirectory
                    auth
                    repository
                    commit
                    sourceDir
                    targetDir
        }
mkEffects
    :: Auth -> MPFS ClientM -> Maybe TokenId -> Effects ClientM
mkEffects auth mpfs tk = do
    Effects
        { mpfsGetFacts = getFacts mpfs tk
        , mpfsGetTestRuns = getTestRuns mpfs tk
        , mpfsGetTokenRequests = getTokenRequests mpfs tk
        , githubEffects = mkGithubEffects auth
        , withSystemTempDirectory = \template action ->
            control
                (\run -> Temp.withSystemTempDirectory template (run . action))
        , withCurrentDirectory = \dir action ->
            control
                (\run -> System.withCurrentDirectory dir (run action))
        , writeTextFile = \path content -> liftIO $ T.writeFile path content
        , directoryExists = \(Directory path) -> liftIO $ do
            exists <- System.doesDirectoryExist path
            if exists
                then Just <$> System.getPermissions path
                else return Nothing
        , fileExists = \path -> liftIO $ do
            exists <- System.doesFileExist path
            if exists
                then Just <$> System.getPermissions path
                else return Nothing
        , decodePrivateSSHFile = liftIO . SSH.sshKeyPair
        , dockerComposeConfigure = \d ->
            liftIO
                $ dockerCompose
                    d
                    ["config"]
                <&> ($> ())
        }

data KeyFailure
    = KeyAlreadyExists String
    | KeyDoesNotExist String
    deriving (Show, Eq)

instance Monad m => ToJSON m KeyFailure where
    toJSON = \case
        KeyAlreadyExists key -> object ["keyAlreadyExists" .= key]
        KeyDoesNotExist key -> object ["keyDoesNotExist" .= key]

-- | Validate a change just as an mpf change.
-- * Insert should have a fresh key
-- * Update should have a key that exists
-- * Delete should have a key that exists
insertValidation
    :: forall m k v
     . (Monad m, FromJSON Maybe k, Eq k, Show k, FromJSON Maybe v)
    => Effects m
    -> Change k (OpI v)
    -> Validate KeyFailure m ()
insertValidation Effects{mpfsGetFacts} (Change (Key k) _) = do
    facts :: [Fact k v] <- lift mpfsGetFacts
    when (any (\(Fact k' _ _) -> k' == k) facts)
        $ notValidated
        $ KeyAlreadyExists
        $ show k

deleteValidation
    :: forall m k v
     . (Monad m, FromJSON Maybe k, Eq k, Show k, FromJSON Maybe v)
    => Effects m
    -> Change k (OpD v)
    -> Validate KeyFailure m ()
deleteValidation Effects{mpfsGetFacts} (Change (Key k) _) = do
    facts :: [Fact k v] <- lift mpfsGetFacts
    when (all (\(Fact k' _ _) -> k' /= k) facts)
        $ notValidated
        $ KeyDoesNotExist
        $ show k

updateValidation
    :: forall m k v w
     . (Monad m, FromJSON Maybe k, Eq k, Show k, FromJSON Maybe v)
    => Effects m
    -> Change k (OpU v w)
    -> Validate KeyFailure m ()
updateValidation Effects{mpfsGetFacts} (Change (Key k) _) = do
    facts :: [Fact k v] <- lift mpfsGetFacts
    when (not $ any (\(Fact k' _ _) -> k' == k) facts)
        $ notValidated
        $ KeyDoesNotExist
        $ show k
