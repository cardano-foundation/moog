-- | GitHub identity types shared across the auth modules.
--
-- Slice 1 of #112 introduces only the 'Login' newtype, which pins the
-- public login parameter of 'Lib.GitHub.Auth.TeamCheck.checkTeamMembership'.
-- The @whoami@ boundary that produces a 'Login' is added in a later slice.
module Lib.GitHub.Auth.Identify
    ( Login (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text)

-- | A GitHub login (username). A distinct newtype so it cannot be
-- confused with an 'Lib.GitHub.Auth.TeamCheck.Org' or
-- 'Lib.GitHub.Auth.TeamCheck.TeamSlug' at a call site.
newtype Login = Login {unLogin :: Text}
    deriving stock (Show, Eq, Ord)
    deriving newtype (IsString, ToJSON, FromJSON)
