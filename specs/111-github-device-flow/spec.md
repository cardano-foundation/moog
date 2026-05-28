# Spec — moog#111 GitHub OAuth Device Flow client

## P1 user story

As a moog operator who needs to call the future Antithesis proxy from a laptop or CI runner, I can run a pure Haskell GitHub OAuth Device Flow client with a caller-supplied OAuth App `client_id`, see the GitHub verification URL and user code, approve the request in the browser, and receive an `OAuthToken` value without any moog-specific state or environment-variable dependency.

## Why

The parent epic (#109) needs a GitHub-team-gated Antithesis HTTP proxy. The CLI side must obtain a GitHub user token before later tickets can identify the user (#112), cache the token and expose commands (#114), or authenticate requests through the proxy (#113).

This ticket is the type-defining foundation for that stack. `OAuthToken` and the related newtypes must be stable enough for sibling tickets to import after the first implementation slice lands.

## Authoritative API

```haskell
module Lib.GitHub.Auth.DeviceFlow
    ( ClientId (..)
    , Scope (..)
    , OAuthToken (..)
    , DeviceCodeResponse (..)
    , DeviceFlowError (..)
    , runDeviceFlow
    ) where

newtype ClientId = ClientId Text
newtype Scope = Scope Text
newtype OAuthToken = OAuthToken {unOAuthToken :: ByteString}

data DeviceCodeResponse = DeviceCodeResponse
    { dcVerificationUri :: Text
    , dcUserCode :: Text
    , dcExpiresIn :: Int
    , dcInterval :: Int
    }

data DeviceFlowError
    = ExpiredToken
    | AccessDenied
    | UnexpectedStatus Int Text
    | NetworkError Text

runDeviceFlow
    :: MonadIO m
    => ClientId
    -> [Scope]
    -> (DeviceCodeResponse -> m ())
    -> m (Either DeviceFlowError OAuthToken)
```

## Functional requirements

- **F1** Add `Lib.GitHub.Auth.DeviceFlow` under `src/Lib/GitHub/Auth/DeviceFlow.hs` with exactly the exported public API above.
- **F2** `runDeviceFlow` sends `POST /login/device/code` as form data containing `client_id` and the requested scopes, parses `device_code`, `user_code`, `verification_uri`, `expires_in`, and `interval`, then invokes the caller callback once with the parsed `DeviceCodeResponse`.
- **F3** `runDeviceFlow` polls `POST /login/oauth/access_token` as form data containing `client_id`, `device_code`, and `grant_type=urn:ietf:params:oauth:grant-type:device_code`.
- **F4** Polling waits the server-provided interval between attempts, continues on `authorization_pending`, increases the interval by 5 seconds on `slow_down`, returns `Left ExpiredToken` on `expired_token`, returns `Left AccessDenied` on `access_denied`, and returns `Right (OAuthToken access_token)` on success.
- **F5** Non-2xx HTTP statuses, unrecognized OAuth error strings, and malformed JSON are reported as `Left (UnexpectedStatus status body)` where an HTTP status exists, or `Left (NetworkError text)` when the request cannot be completed or decoded into the expected shape.
- **F6** The library never reads a client id from the environment. `ClientId` is always an explicit parameter.
- **F7** The unit suite uses an embedded WAI mock server and does not call github.com.
- **F8** The live-boundary smoke uses the actual exported API against github.com, prints the `user_code`, waits for browser approval, and prints only the resulting token prefix length evidence: first 4 characters plus total length, never the full token.

## Success criteria

- **S1** Unit tests cover `authorization_pending -> success`, `slow_down -> success`, `expired_token`, `access_denied`, and malformed JSON against an embedded WAI mock server.
- **S2** Unit tests prove the callback receives `verification_uri`, `user_code`, `expires_in`, and `interval` from the mock device-code response.
- **S3** Unit tests prove `client_id` is sent from the `ClientId` parameter, not read from an environment variable.
- **S4** The public API is pinned and announced to the parent epic after the first implementation slice passes its focused unit gate.
- **S5** A tiny Cabal-runnable smoke harness exists for the real OAuth App from #110 and never logs a full token.
- **S6** `gate.sh` passes at every accepted slice boundary.

## Out of scope

- Caching the token to disk.
- Calling GitHub APIs with the token.
- CLI UX for invoking the flow.
- Proxy authentication or team membership checks.
