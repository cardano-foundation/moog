module Oracle.Validate.Requests.ManageWhiteList
    ( validateAddWhiteListed
    , validateRemoveWhiteListed
    , UpdateWhiteListFailure (..)
    )
where

import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans (..))
import Core.Types.Basic (Owner, Platform (..), Repository)
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation (Op (..))
import Effects
    ( Effects (..)
    , GithubEffects (..)
    , KeyFailure
    , deleteValidation
    , insertValidation
    )
import Lib.GitHub qualified as Github
import Lib.JSON.Canonical.Extra (object, (.=))
import Oracle.Validate.Types
    ( Validate
    , Validated (..)
    , mapFailure
    , throwFalse
    , throwLeft
    )
import Submitting (WalletError)
import Text.JSON.Canonical (ToJSON (..))
import User.Agent.Types (WhiteListKey (..))

data UpdateWhiteListFailure
    = WhiteListPlatformUnsupported Platform
    | WhiteListRepositoryNotInThePlatform Repository
    | WhiteListRepositoryKeyValidation KeyFailure
    | WhiteListGithubFailed Github.GithubResponseStatusCodeError
    | WhiteListAgentNotRecognized Owner
    | WhiteListConfigNotAvailable
    | WhiteListWalletError WalletError
    deriving (Show, Eq)

instance Monad m => ToJSON m UpdateWhiteListFailure where
    toJSON (WhiteListPlatformUnsupported platform) =
        object ["error" .= ("Platform is missing: " ++ show platform)]
    toJSON (WhiteListRepositoryNotInThePlatform repo) =
        object
            ["error" .= ("Repository is not in the platform: " ++ show repo)]
    toJSON (WhiteListRepositoryKeyValidation keyFailure) =
        object
            ["error" .= ("Repository key validation failed: " ++ show keyFailure)]
    toJSON (WhiteListGithubFailed err) =
        object ["error" .= ("GitHub error: " ++ show err)]
    toJSON (WhiteListAgentNotRecognized agent) =
        object ["error" .= ("Agent not recognized: " ++ show agent)]
    toJSON WhiteListConfigNotAvailable =
        object
            ["error" .= ("Token configuration is not available yet" :: String)]
    toJSON (WhiteListWalletError err) =
        object ["error" .= ("Wallet error: " ++ show err)]

validateAgent
    :: Monad m
    => Owner
    -> Owner
    -> Validate UpdateWhiteListFailure m Validated
validateAgent agent submitter = do
    throwFalse (agent == submitter)
        $ WhiteListAgentNotRecognized submitter

validateAddWhiteListed
    :: Monad m
    => Effects m
    -> Owner
    -> Owner
    -> Change WhiteListKey (OpI ())
    -> Validate UpdateWhiteListFailure m Validated
validateAddWhiteListed
    v@Effects
        { githubEffects = GithubEffects{githubRepositoryExists}
        }
    agent
    submitter
    c@(Change (Key (WhiteListKey platform repo)) _) = do
        void $ validateAgent agent submitter
        mapFailure WhiteListRepositoryKeyValidation $ insertValidation v c
        void
            $ throwFalse (platform == Platform "github")
            $ WhiteListPlatformUnsupported platform
        eexists <- lift $ githubRepositoryExists repo
        exist <- throwLeft WhiteListGithubFailed eexists
        throwFalse exist
            $ WhiteListRepositoryNotInThePlatform repo

validateRemoveWhiteListed
    :: Monad m
    => Effects m
    -> Owner
    -> Owner
    -> Change WhiteListKey (OpD ())
    -> Validate UpdateWhiteListFailure m Validated
validateRemoveWhiteListed
    v
    agent
    submitter
    c@(Change (Key (WhiteListKey platform _repo)) _) = do
        void $ validateAgent agent submitter
        mapFailure WhiteListRepositoryKeyValidation $ deleteValidation v c
        throwFalse (platform == Platform "github")
            $ WhiteListPlatformUnsupported platform
