{-# LANGUAGE StrictData #-}

module Effects.RegisterUser
    ( PublicKeyFailure (..)
    , inspectPublicKeyTemplate
    , inspectPublicKey
    , analyzeKeys
    ) where

import Core.Types.Basic (GithubUsername)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import GitHub (Auth)
import Lib.GitHub (GithubResponseError, githubUserPublicKeys)
import Lib.SSH.Public (SSHPublicKey (..))
import Text.JSON.Canonical (ToJSON (..))

data PublicKeyFailure
    = NoPublicKeyFound
    | NoEd25519KeyFound
    | NoEd25519KeyMatch
    | GithubError String
    deriving (Eq, Show)

instance Monad m => ToJSON m PublicKeyFailure where
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
    -> Maybe PublicKeyFailure
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
    -> Maybe PublicKeyFailure
analyzePublicKeyResponse pubkeyToValidate = \case
    Left err -> Just $ GithubError $ show err
    Right resp ->
        analyzeKeys pubkeyToValidate
            $ SSHPublicKey . T.unpack <$> resp

inspectPublicKeyTemplate
    :: GithubUsername
    -> SSHPublicKey
    -> (GithubUsername -> IO (Either GithubResponseError [Text]))
    -> IO (Maybe PublicKeyFailure)
inspectPublicKeyTemplate username pubKeyExpected requestPublicKeysForUser = do
    resp <- requestPublicKeysForUser username
    pure $ analyzePublicKeyResponse pubKeyExpected resp

inspectPublicKey
    :: Auth
    -> GithubUsername
    -> SSHPublicKey
    -> IO (Maybe PublicKeyFailure)
inspectPublicKey auth username pubKeyExpected =
    inspectPublicKeyTemplate
        username
        pubKeyExpected
        $ githubUserPublicKeys auth
