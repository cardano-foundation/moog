-- | Public GitHub team-membership API.
--
-- 'checkTeamMembership' reports whether a GitHub login is a member of a
-- given team within an organisation, mapping the REST responses to a
-- 'MembershipResult'.
module Lib.GitHub.Auth.TeamCheck
    ( Org (..)
    , TeamSlug (..)
    , MembershipResult (..)
    , Login (..)
    , checkTeamMembership
    ) where

import Lib.GitHub.Auth.DeviceFlow (OAuthToken)
import Lib.GitHub.Auth.Identify (Login (..))
import Lib.GitHub.Auth.TeamCheck.Internal
    ( MembershipResult (..)
    , Org (..)
    , TeamSlug (..)
    , checkTeamMembershipWith
    , githubTeamCheckEndpoint
    )

-- | Check whether @login@ is a member of @team@ within @org@, using the
-- supplied OAuth token against the live GitHub REST API.
checkTeamMembership
    :: OAuthToken
    -> Org
    -> TeamSlug
    -> Login
    -> IO MembershipResult
checkTeamMembership token org team login =
    checkTeamMembershipWith
        githubTeamCheckEndpoint
        token
        org
        team
        (unLogin login)
