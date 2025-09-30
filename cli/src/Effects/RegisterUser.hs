{-# LANGUAGE StrictData #-}

module Effects.RegisterUser
    ( SSHPublicKeyFailure (..)
    , inspectPublicKeyTemplate
    , inspectPublicKey
    , analyzeKeys
    , inspectVKey
    , VKey (..)
    , VKeyFailure (..)
    , analyzeVKeyResponse
    ) where

import Core.Types.Basic (GithubUsername)
import Core.Types.VKey (VKey (..))
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import GitHub (Auth)
import Lib.GitHub
    ( GetGithubFileFailure
    , GithubResponseError
    , githubGetAntiCLIVKey
    , githubUserPublicKeys
    )
import Lib.SSH.Public (SSHPublicKey (..))
import Text.JSON.Canonical (ToJSON (..))

data SSHPublicKeyFailure
    = NoPublicKeyFound
    | NoEd25519KeyFound
    | NoEd25519KeyMatch
    | GithubError String
    deriving (Eq, Show)

instance Monad m => ToJSON m SSHPublicKeyFailure where
    toJSON = \case
        NoPublicKeyFound ->
            toJSON
                ("The user does not have any public key exposed in Github." :: String)
        NoEd25519KeyFound ->
            toJSON
                ( "The user is expected to have public key with '"
                    <> expectedPrefix
                    <> "' exposed. And none was found"
                    :: String
                )
        NoEd25519KeyMatch ->
            toJSON
                ( "The user does not have the specified Ed25519 public key exposed in Github."
                    :: String
                )
        GithubError err ->
            toJSON
                ("The following github error was encountered: " <> err :: String)

expectedPrefix :: String
expectedPrefix = "ssh-ed25519 "

analyzeKeys
    :: SSHPublicKey
    -> [SSHPublicKey]
    -> Maybe SSHPublicKeyFailure
analyzeKeys pubkeyToValidate resp
    | null resp = Just NoPublicKeyFound
    | hasNotTheKey resp = Just NoEd25519KeyMatch
    | otherwise = Nothing
  where
    hasNotTheKey =
        L.notElem pubkeyToValidate

analyzePublicKeyResponse
    :: SSHPublicKey
    -> Either GithubResponseError [Text]
    -> Maybe SSHPublicKeyFailure
analyzePublicKeyResponse pubkeyToValidate = \case
    Left err -> Just $ GithubError $ show err
    Right resp ->
        analyzeKeys pubkeyToValidate
            $ SSHPublicKey . T.unpack <$> resp

inspectPublicKeyTemplate
    :: GithubUsername
    -> SSHPublicKey
    -> (GithubUsername -> IO (Either GithubResponseError [Text]))
    -> IO (Maybe SSHPublicKeyFailure)
inspectPublicKeyTemplate username pubKeyExpected requestPublicKeysForUser = do
    resp <- requestPublicKeysForUser username
    pure $ analyzePublicKeyResponse pubKeyExpected resp

inspectPublicKey
    :: Auth
    -> GithubUsername
    -> SSHPublicKey
    -> IO (Maybe SSHPublicKeyFailure)
inspectPublicKey auth username pubKeyExpected =
    inspectPublicKeyTemplate
        username
        pubKeyExpected
        $ githubUserPublicKeys auth

data VKeyFailure
    = VKeyMismatch VKey
    | VKeyGithubError GetGithubFileFailure
    deriving (Eq, Show)

inspectVKey
    :: Auth
    -> GithubUsername
    -> VKey
    -> IO (Maybe VKeyFailure)
inspectVKey auth username expectedVKey = do
    resp <- githubGetAntiCLIVKey auth username
    pure $ analyzeVKeyResponse expectedVKey $ VKey <$> resp

analyzeVKeyResponse
    :: VKey -> Either GetGithubFileFailure VKey -> Maybe VKeyFailure
analyzeVKeyResponse _expected (Left err) = Just $ VKeyGithubError err
analyzeVKeyResponse expected (Right resp)
    | resp == expected = Nothing
    | otherwise = Just $ VKeyMismatch resp
