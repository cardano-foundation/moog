-- | Public OAuth constants for the Moog Antithesis CLI.
module User.Antithesis.Constants
    ( moogOAuthClientId
    , moogOAuthScopes
    )
where

import Lib.GitHub.Auth.DeviceFlow (ClientId (..), Scope (..))

moogOAuthClientId :: ClientId
moogOAuthClientId = ClientId "Ov23liVVFVtdBez1QDxq"

moogOAuthScopes :: [Scope]
moogOAuthScopes = [Scope "read:org"]
