# Spec - moog#112 GitHub identity and team membership checks

## P1 user story

As the Antithesis proxy boundary, moog can take an `OAuthToken` from the device-flow client, identify the GitHub user who owns it, and answer whether that login is an active member of the `pragma/antithesis` team, including a concrete SSO authorization URL when GitHub requires organization SSO.

## Why

The parent epic (#109) gates Antithesis requests by GitHub team membership. Ticket #111 already pinned `OAuthToken`; this ticket pins the identity and membership API consumed by the proxy daemon in #113. This layer is intentionally stateless so #113 owns token-prefix caching at the proxy boundary.

## Authoritative API

```haskell
module Lib.GitHub.Auth.Identify
    ( Login (..)
    , GitHubError (..)
    , whoami
    ) where

newtype Login = Login { unLogin :: Text }

whoami :: OAuthToken -> IO (Either GitHubError Login)
```

```haskell
module Lib.GitHub.Auth.TeamCheck
    ( Org (..)
    , TeamSlug (..)
    , Login (..)
    , MembershipResult (..)
    , checkTeamMembership
    ) where

newtype Org = Org Text
newtype TeamSlug = TeamSlug Text

data MembershipResult
    = Active
    | Pending
    | NotMember
    | TokenInvalid
    | SSORequired { url :: Text }
    | OtherError { status :: Int, body :: Text }

checkTeamMembership
    :: OAuthToken -> Org -> TeamSlug -> Login -> IO MembershipResult
```

`OAuthToken` is imported from `Lib.GitHub.Auth.DeviceFlow`; this ticket must not redefine it.
`Login` is a moog-owned newtype because `github-0.30.0.2` does not expose the issue-body shorthand `GH.Login`. `Lib.GitHub.Auth.TeamCheck` re-exports `Login` so #113 can import the full team-check API surface from one module.

## Functional requirements

- **F1** Add `Lib.GitHub.Auth.Identify` with `whoami`, backed by `GET https://api.github.com/user`.
- **F2** Add `Lib.GitHub.Auth.TeamCheck` with `Org`, `TeamSlug`, `Login`, `MembershipResult`, and `checkTeamMembership`, backed by `GET /orgs/{org}/teams/{team_slug}/memberships/{login}`.
- **F3** Convert `OAuthToken` into the `github` package auth representation expected by the existing project, without logging or exposing the token bytes.
- **F4** Map membership responses as follows: `200 state=active` to `Active`, `200 state=pending` to `Pending`, `404` to `NotMember`, `401` to `TokenInvalid`, `403` with `X-GitHub-SSO` URL to `SSORequired url`, and all other failures to `OtherError status body`.
- **F5** Surface the SSO URL verbatim from the `X-GitHub-SSO` header when present.
- **F6** Keep this layer stateless: no cache, no token persistence, no retry policy beyond the underlying HTTP client.
- **F7** Unit tests use embedded fixture HTTP responses and never call github.com.
- **F8** Add an operator-run live smoke that accepts a real token, calls `whoami`, calls `checkTeamMembership pragma antithesis <login>`, and redacts token material from all output.

## Success criteria

- **S1** Unit tests cover every `MembershipResult` constructor: active, pending, not member, invalid token, SSO required, and generic other error.
- **S2** Unit tests verify `whoami` returns the expected `Login` for a fixture `/user` response and reports an error for a non-success response.
- **S3** The public `MembershipResult` type is pinned and announced to the parent epic after the first implementation slice passes its focused gate.
- **S4** The live-boundary smoke is runnable with the real OAuth App client id from #110 and a real `pragma/antithesis` member token; the operator-run proof shows `whoami` returns the expected login and membership returns `Active`.
- **S5** `./gate.sh` passes at every accepted implementation slice boundary.

## Out of scope

- Device Flow token acquisition; #111 owns that.
- Proxy daemon auth, HTTP routes, or caching; #113 owns those.
- CLI token storage and commands; #114 owns those.
- GitHub retries beyond the HTTP client defaults.
