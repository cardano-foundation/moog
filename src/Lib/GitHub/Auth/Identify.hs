-- | GitHub identity types and the @whoami@ boundary.
--
-- Slice 1 of #112 introduced the 'Login' newtype, which pins the public
-- login parameter of 'Lib.GitHub.Auth.TeamCheck.checkTeamMembership'.
-- This module re-exports 'Login' (now defined in
-- "Lib.GitHub.Auth.Identify.Internal") alongside 'whoami', which
-- resolves the login of the token holder via @GET https://api.github.com/user@.
module Lib.GitHub.Auth.Identify
    ( Login (..)
    , GitHubError (..)
    , whoami
    ) where

import Lib.GitHub.Auth.DeviceFlow (OAuthToken)
import Lib.GitHub.Auth.Identify.Internal
    ( GitHubError (..)
    , Login (..)
    , githubIdentifyEndpoint
    , whoamiWith
    )

-- | Look up the login of the authenticated user, using the supplied
-- OAuth token against the live GitHub REST API.
whoami :: OAuthToken -> IO (Either GitHubError Login)
whoami = whoamiWith githubIdentifyEndpoint
