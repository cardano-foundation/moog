# Plan - moog#112

## Tech stack

- Haskell, Cabal, and the existing project warning profile.
- Existing `github`, `http-client`, `http-client-tls`, `http-types`, `aeson`, `bytestring`, and `text` dependencies.
- Unit tests in the existing `unit-tests` hspec suite, reusing the embedded WAI/Warp mock-server pattern from #111.
- `OAuthToken` imported from `Lib.GitHub.Auth.DeviceFlow`.

Dependency-manifest edits are behavior-changing and belong to the driver/navigator pair for the implementation slice that needs them.

## Module layout

```text
src/Lib/GitHub/Auth/TeamCheck.hs                 # NEW public membership API.
src/Lib/GitHub/Auth/TeamCheck/Internal.hs        # NEW testable endpoints and response mapping.
test/Lib/GitHub/Auth/TeamCheckSpec.hs            # NEW fixture tests for MembershipResult.
src/Lib/GitHub/Auth/Identify.hs                  # NEW public whoami API.
src/Lib/GitHub/Auth/Identify/Internal.hs         # NEW testable endpoint/client hook.
test/Lib/GitHub/Auth/IdentifySpec.hs             # NEW fixture tests for /user.
app/Moog/GitHub/AuthSmoke.hs                     # NEW live-boundary smoke helper.
app/moog-github-auth-smoke.hs                    # NEW executable entry point.
test/Lib/GitHub/Auth/AuthSmokeSpec.hs            # NEW token-output redaction tests.
```

Public consumers should import only `Lib.GitHub.Auth.TeamCheck`, `Lib.GitHub.Auth.Identify`, and `Lib.GitHub.Auth.DeviceFlow`.

## Response mapping

`checkTeamMembership` must produce exactly these outcomes:

- `200` with JSON `state = "active"`: `Active`.
- `200` with JSON `state = "pending"`: `Pending`.
- `404`: `NotMember`.
- `401`: `TokenInvalid`.
- `403` with an `X-GitHub-SSO` header containing an `https://github.com/orgs/<org>/sso?...` URL: `SSORequired url`.
- Any other status, malformed successful response, unknown state, or GitHub error without a more specific mapping: `OtherError status body`.

The SSO parser should preserve the authorization URL exactly as GitHub sent it, excluding only header metadata such as `required; url=`.

## Slices

### Slice 0 - Orchestration bootstrap

Owned by the ticket-orchestrator. Add spec, plan, and tasks; use the inherited repository `gate.sh`; open the draft PR. No production or test behavior changes.

### Slice 1 - Team membership API and fixture matrix

Add `Lib.GitHub.Auth.TeamCheck`, a testable internal module, cabal module declarations, and fixture tests for every `MembershipResult` constructor. This is the type-pinning slice for #113.

RED: add mock-server tests that fail because the module/API does not exist.

GREEN: implement the endpoint request, token auth, response decoding, status mapping, and SSO header URL extraction.

Bisect-safe because the new module is additive and covered by focused fixture tests.

After this slice is accepted and pushed, append `NOTE RELEASE: membership-result-type-pinned at <commit-or-PR-url>` to `/tmp/epic-109/moog-112/STATUS.md`.

### Slice 2 - GitHub identity API

Add `Lib.GitHub.Auth.Identify`, a testable internal module, cabal module declarations, and fixture tests for `GET /user`.

RED: add tests proving a fixture user response returns the expected `GH.Login` and a non-success response returns `Left GitHubError`.

GREEN: implement `whoami` using the shared OAuth token conversion and GitHub user response parsing.

Bisect-safe because `whoami` is additive and does not change team-check behavior.

### Slice 3 - Live-boundary auth smoke

Add a Cabal-runnable smoke executable that accepts a token and expected login, calls `whoami`, then calls `checkTeamMembership (Org "pragma") (TeamSlug "antithesis") <login>`, and reports success only for matching login plus `Active` membership. Output must not include the full token.

RED: add unit-level output-safety or argument-parsing tests for the smoke helper.

GREEN: implement the smoke helper and executable stanza.

After the slice lands locally, raise a parent Q-file asking the operator to run the live smoke with the real OAuth App/token. Do not block earlier implementation slices on this operator-run proof.

## Gate

This branch inherits `gate.sh` from `origin/main`; it currently runs:

```bash
git diff --check
nix run .#unit-tests
```

The focused commands for slices use `nix develop --quiet -c just unit "<match>"` while developing, followed by `./gate.sh` before commit and again during orchestrator acceptance.

The live-boundary smoke is not part of `gate.sh` because it needs an operator-held token and real GitHub SSO/team state.

## Risks and mitigations

- **GitHub API surface uncertainty.** Keep internal helpers injectable so fixture tests prove wire behavior without depending on github.com.
- **SSO header parsing.** Test the exact `X-GitHub-SSO` shape with a URL-bearing fixture.
- **Token leakage.** Smoke tests must prove only redacted token evidence is rendered.
- **Cross-ticket type drift.** Treat `MembershipResult` as pinned after Slice 1. Any later constructor/signature change requires a parent Q-file before continuing.
