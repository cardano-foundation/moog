{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unless" #-}
module Validation
    ( Validation (..)
    , KeyFailure (..)
    , mkValidation
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
    , PublicKeyHash
    , Repository
    , TokenId
    , Username
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..), JSFact, parseFacts)
import Core.Types.Operation (Op (..))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as T
import GitHub (Auth)
import Lib.GitHub qualified as GitHub
import Lib.JSON.Canonical.Extra (object, (.=))
import Lib.SSH.Private (KeyPair, SSHClient, WithSelector (..))
import Lib.SSH.Private qualified as SSH
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
import Validation.DownloadFile
    ( DownloadedFileFailure
    , inspectDownloadedFile
    )
import Validation.RegisterRole
    ( RepositoryRoleFailure
    , inspectRepoRoleForUser
    )
import Validation.RegisterUser
    ( PublicKeyFailure
    , inspectPublicKey
    )

-- | Abstract the side effects necessary for validation.
data Validation m = Validation
    { mpfsGetFacts
        :: forall k v
         . (FromJSON Maybe k, FromJSON Maybe v)
        => m [Fact k v]
    , mpfsGetTestRuns :: m [TestRun]
    , mpfsGetTokenRequests :: m [RequestZoo]
    , githubCommitExists
        :: Repository
        -> Commit
        -> m (Either GitHub.GithubResponseError Bool)
    , githubDirectoryExists
        :: Repository
        -> Commit
        -> Directory
        -> m (Either GitHub.GithubResponseStatusCodeError Bool)
    , githubUserPublicKeys
        :: Username
        -> PublicKeyHash
        -> m (Maybe PublicKeyFailure)
    , githubRepositoryExists
        :: Repository
        -> m (Either GitHub.GithubResponseStatusCodeError Bool)
    , githubRepositoryRole
        :: Username
        -> Repository
        -> m (Maybe RepositoryRoleFailure)
    , githubGetFile
        :: Repository
        -> Maybe Commit
        -> FileName
        -> m (Either DownloadedFileFailure Text)
    , githubDownloadDirectory
        :: Repository
        -> Maybe Commit
        -> Directory
        -> Directory
        -> m (Either GitHub.GetGithubFileFailure ())
    , withSystemTempDirectory
        :: forall a
         . String
        -> (FilePath -> m a)
        -> m a
    , writeTextFile :: FilePath -> Text -> m ()
    , withCurrentDirectory :: forall a. FilePath -> m a -> m a
    , directoryExists :: Directory -> m (Maybe Permissions)
    , decodePrivateSSHFile :: SSHClient 'WithSelector -> m (Maybe KeyPair)
    }

hoistValidation
    :: (MonadTransControl t, Monad m)
    => Validation m
    -> Validation (t m)
hoistValidation
    Validation
        { mpfsGetFacts
        , mpfsGetTestRuns
        , mpfsGetTokenRequests
        , githubCommitExists
        , githubDirectoryExists
        , githubUserPublicKeys
        , githubRepositoryExists
        , githubRepositoryRole
        , githubGetFile
        , githubDownloadDirectory
        , withSystemTempDirectory
        , withCurrentDirectory
        , writeTextFile
        , directoryExists
        , decodePrivateSSHFile
        } =
        Validation
            { mpfsGetFacts = f mpfsGetFacts
            , mpfsGetTestRuns = f mpfsGetTestRuns
            , mpfsGetTokenRequests = f mpfsGetTokenRequests
            , githubCommitExists =
                \repo commit -> f $ githubCommitExists repo commit
            , githubDirectoryExists =
                \repo commit dir -> f $ githubDirectoryExists repo commit dir
            , githubUserPublicKeys =
                \username publicKey -> f $ githubUserPublicKeys username publicKey
            , githubRepositoryExists =
                f . githubRepositoryExists
            , githubRepositoryRole =
                \username repository -> f $ githubRepositoryRole username repository
            , githubGetFile =
                \repository commit filename -> f $ githubGetFile repository commit filename
            , githubDownloadDirectory =
                \repository commit sourceDir targetDir ->
                    f
                        $ githubDownloadDirectory repository commit sourceDir targetDir
            , withSystemTempDirectory =
                \template action -> controlT
                    $ \run -> withSystemTempDirectory template (run . action)
            , withCurrentDirectory = \dir action -> controlT
                $ \run -> withCurrentDirectory dir (run action)
            , writeTextFile = \path content -> f $ writeTextFile path content
            , directoryExists = f . directoryExists
            , decodePrivateSSHFile = f . decodePrivateSSHFile
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
getTestRuns mpfs tk = mapMaybe (\(Fact k _ :: JSFact) -> fromJSON k) <$> getFacts mpfs tk

getTokenRequests
    :: Monad m => MPFS m -> Maybe TokenId -> m [RequestZoo]
getTokenRequests mpfs tk = case tk of
    Nothing -> pure []
    Just tokenId -> do
        mtoken <- fromJSON <$> mpfsGetToken mpfs tokenId
        pure $ maybe [] (fmap runIdentity . tokenRequests) mtoken

mkValidation
    :: Auth -> MPFS ClientM -> Maybe TokenId -> Validation ClientM
mkValidation auth mpfs tk = do
    Validation
        { mpfsGetFacts = getFacts mpfs tk
        , mpfsGetTestRuns = getTestRuns mpfs tk
        , mpfsGetTokenRequests = getTokenRequests mpfs tk
        , githubCommitExists = \repository commit ->
            liftIO $ GitHub.githubCommitExists auth repository commit
        , githubDirectoryExists = \repository commit dir ->
            liftIO $ GitHub.githubDirectoryExists auth repository commit dir
        , githubUserPublicKeys = \username publicKey ->
            liftIO $ inspectPublicKey auth username publicKey
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
        , decodePrivateSSHFile = liftIO . SSH.sshKeyPair
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
    => Validation m
    -> Change k (OpI v)
    -> Validate KeyFailure m ()
insertValidation Validation{mpfsGetFacts} (Change (Key k) _) = do
    facts :: [Fact k v] <- lift mpfsGetFacts
    when (any (\(Fact k' _) -> k' == k) facts)
        $ notValidated
        $ KeyAlreadyExists
        $ show k

deleteValidation
    :: forall m k v
     . (Monad m, FromJSON Maybe k, Eq k, Show k, FromJSON Maybe v)
    => Validation m
    -> Change k (OpD v)
    -> Validate KeyFailure m ()
deleteValidation Validation{mpfsGetFacts} (Change (Key k) _) = do
    facts :: [Fact k v] <- lift mpfsGetFacts
    when (all (\(Fact k' _) -> k' /= k) facts)
        $ notValidated
        $ KeyDoesNotExist
        $ show k

updateValidation
    :: forall m k v w
     . (Monad m, FromJSON Maybe k, Eq k, Show k, FromJSON Maybe v)
    => Validation m
    -> Change k (OpU v w)
    -> Validate KeyFailure m ()
updateValidation Validation{mpfsGetFacts} (Change (Key k) _) = do
    facts :: [Fact k v] <- lift mpfsGetFacts
    when (not $ any (\(Fact k' _) -> k' == k) facts)
        $ notValidated
        $ KeyDoesNotExist
        $ show k
