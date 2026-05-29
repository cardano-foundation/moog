{-# LANGUAGE OverloadedStrings #-}

-- | Operator smoke for the live GitHub auth boundary.
--
-- This module exposes pure helpers (argument parsing, token-evidence
-- redaction, and outcome rendering) so they can be unit-tested without
-- touching github.com, alongside 'main', which performs the live
-- @whoami@ and team-membership calls.
--
-- The expected operator invocation is:
--
-- > MOOG_GITHUB_OAUTH_TOKEN=<token> \
-- >   nix run .#moog-github-auth-smoke -- --expected-login <login>
module Moog.GitHub.AuthSmoke
    ( SmokeConfig (..)
    , parseSmokeArgs
    , tokenFromEnv
    , renderTokenEvidence
    , renderLoginCheck
    , renderMembershipCheck
    , main
    )
where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import Lib.GitHub.Auth.Identify
    ( GitHubError (..)
    , Login (..)
    , whoami
    )
import Lib.GitHub.Auth.TeamCheck
    ( MembershipResult (..)
    , Org (..)
    , TeamSlug (..)
    , checkTeamMembership
    )
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)

-- | Resolved smoke configuration: the login the token must resolve to,
-- and the org/team whose membership is checked.
data SmokeConfig = SmokeConfig
    { expectedLogin :: Login
    , org :: Org
    , team :: TeamSlug
    }
    deriving stock (Eq, Show)

-- | Name of the environment variable carrying the OAuth token.
tokenEnvVar :: String
tokenEnvVar = "MOOG_GITHUB_OAUTH_TOKEN"

-- | Parse the smoke arguments. @--expected-login@ is required; @--org@
-- and @--team@ default to @pragma-org@ and @antithesis-access@.
parseSmokeArgs :: [String] -> Either Text SmokeConfig
parseSmokeArgs =
    go Nothing (Org "pragma-org") (TeamSlug "antithesis-access")
  where
    go mLogin o t args = case args of
        [] -> case mLogin of
            Just l ->
                Right SmokeConfig{expectedLogin = l, org = o, team = t}
            Nothing -> Left usage
        ("--expected-login" : v : rest) ->
            go (Just (Login (T.pack v))) o t rest
        ("--org" : v : rest) ->
            go mLogin (Org (T.pack v)) t rest
        ("--team" : v : rest) ->
            go mLogin o (TeamSlug (T.pack v)) rest
        (other : _) ->
            Left $
                "unrecognised or incomplete argument: "
                    <> T.pack other
                    <> "\n"
                    <> usage
    usage =
        "usage: moog-github-auth-smoke --expected-login <login>"
            <> " [--org <org>] [--team <team>]"

-- | Read the OAuth token from the environment value, failing closed
-- when it is unset or empty. The token bytes never appear in the error.
tokenFromEnv :: Maybe String -> Either Text OAuthToken
tokenFromEnv mValue = case mValue of
    Just value
        | not (null value) ->
            Right (OAuthToken (T.encodeUtf8 (T.pack value)))
    _ -> Left $ T.pack tokenEnvVar <> " is not set"

-- | Render redacted evidence of the token: its 4-byte prefix and byte
-- length only, never the full secret.
renderTokenEvidence :: OAuthToken -> Text
renderTokenEvidence (OAuthToken token) =
    T.decodeUtf8 (BS.take 4 token)
        <> " len="
        <> T.pack (show (BS.length token))

-- | Render the @whoami@ outcome against the expected login. 'Right'
-- carries a confirmation message; 'Left' carries a failure message for
-- a transport/HTTP error or a login mismatch.
renderLoginCheck
    :: Login
    -> Either GitHubError Login
    -> Either Text Text
renderLoginCheck expected result = case result of
    Left (GitHubError code _) ->
        Left $ "whoami failed with status " <> T.pack (show code)
    Right actual
        | actual == expected ->
            Right $ "identity confirmed: " <> unLogin actual
        | otherwise ->
            Left $
                "expected login "
                    <> unLogin expected
                    <> " but token resolves to "
                    <> unLogin actual

-- | Render the membership outcome. Only 'Active' membership succeeds;
-- every other result is a failure message.
renderMembershipCheck :: MembershipResult -> Either Text Text
renderMembershipCheck result = case result of
    Active -> Right "active team membership confirmed"
    Pending -> Left "membership is pending, not active"
    NotMember -> Left "login is not a member of the team"
    TokenInvalid -> Left "token was rejected by GitHub"
    SSORequired ssoUrl ->
        Left $ "SAML SSO authorisation required: " <> ssoUrl
    OtherError code raw ->
        Left $
            "unexpected membership response: status "
                <> T.pack (show code)
                <> " "
                <> raw

-- | Run the live auth smoke: confirm the token resolves to the expected
-- login, then confirm active membership of the configured team. Exits
-- non-zero (via 'die') on any failure, printing only redacted evidence.
main :: IO ()
main = do
    config <-
        either (die . T.unpack) pure . parseSmokeArgs =<< getArgs
    token <-
        either (die . T.unpack) pure . tokenFromEnv
            =<< lookupEnv tokenEnvVar
    putStrLn $ "Token evidence: " <> T.unpack (renderTokenEvidence token)
    loginResult <- whoami token
    case renderLoginCheck (expectedLogin config) loginResult of
        Left err -> die $ T.unpack err
        Right msg -> putStrLn $ T.unpack msg
    membership <-
        checkTeamMembership
            token
            (org config)
            (team config)
            (expectedLogin config)
    case renderMembershipCheck membership of
        Left err -> die $ T.unpack err
        Right msg -> putStrLn $ T.unpack msg
