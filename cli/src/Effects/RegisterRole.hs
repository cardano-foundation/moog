{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Effects.RegisterRole
    ( RepositoryRoleFailure (..)
    , inspectRepoRoleForUserTemplate
    , inspectRepoRoleForUser
    ) where

import Core.Types.Basic (Repository, Username (..))
import Data.List qualified as L
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import GitHub (Auth)
import Lib.GitHub (GetGithubFileFailure, githubGetCodeOwnersFile)
import Lib.JSON.Canonical.Extra (object, (.=))
import Text.JSON.Canonical (ToJSON (..))

data RepositoryRoleFailure
    = NoRoleEntryInCodeowners
    | NoUsersAssignedToRoleInCodeowners
    | NoUserInCodeowners
    | GithubGetError GetGithubFileFailure
    deriving (Eq, Show)

instance Monad m => ToJSON m RepositoryRoleFailure where
    toJSON = \case
        NoRoleEntryInCodeowners ->
            toJSON ("No role entry in CODEOWNERS file." :: Text)
        NoUsersAssignedToRoleInCodeowners ->
            toJSON ("No users assigned to role in CODEOWNERS file." :: Text)
        NoUserInCodeowners ->
            toJSON ("No user in CODEOWNERS file." :: Text)
        GithubGetError failure ->
            object ["githubGetError" .= show failure]

-- In order to verify the role of the userX CODEOWNERS file is downloaded with
-- the expectation there a line:
-- role: user1 user2 .. userX .. userN
analyzeResponseCodeownersFile
    :: Username
    -> Either GetGithubFileFailure Text
    -> Maybe RepositoryRoleFailure
analyzeResponseCodeownersFile (Username user) = \case
    Left failure ->
        Just $ GithubGetError failure
    Right file ->
        if null (lineWithRole file)
            then
                Just NoRoleEntryInCodeowners
            else
                if users file == [Nothing]
                    then
                        Just NoUsersAssignedToRoleInCodeowners
                    else
                        if foundUser file == [[]]
                            then
                                Just NoUserInCodeowners
                            else
                                Nothing
  where
    strBS = "antithesis"
    lineWithRole file = L.filter (T.isPrefixOf strBS) (T.lines file)
    colon = "antithesis" <> ": "
    getUsersWithRole = T.stripPrefix colon
    users file =
        getUsersWithRole
            <$> L.take 1 (lineWithRole file)
    foundUser file =
        L.filter (== ("@" <> T.pack user)) . T.words
            <$> catMaybes (users file)

inspectRepoRoleForUserTemplate
    :: Username
    -> Repository
    -> (Repository -> IO (Either GetGithubFileFailure Text))
    -> IO (Maybe RepositoryRoleFailure)
inspectRepoRoleForUserTemplate username repo downloadCodeownersFile = do
    resp <- downloadCodeownersFile repo
    pure $ analyzeResponseCodeownersFile username resp

inspectRepoRoleForUser
    :: Auth
    -> Username
    -> Repository
    -> IO (Maybe RepositoryRoleFailure)
inspectRepoRoleForUser auth username repo =
    inspectRepoRoleForUserTemplate
        username
        repo
        $ githubGetCodeOwnersFile auth
