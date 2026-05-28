# Plan — moog#111

## Tech stack

- Haskell, Cabal, GHC pinned by the existing project.
- Existing `http-client`, `http-client-tls`, `http-types`, `aeson`, `bytestring`, `text`, and `transformers` dependencies for the client.
- Unit tests in the existing `unit-tests` test-suite with hspec.
- New test-only dependencies for the embedded mock server: `wai`, `warp`, and `unordered-containers` or equivalent request-form parsing support if the implementation needs it.

The dependency-manifest edits are behavior-changing and must be made by the driver/navigator pair, not the orchestrator.

## Module layout

```text
src/Lib/GitHub/Auth/DeviceFlow.hs                 # NEW public API.
src/Lib/GitHub/Auth/DeviceFlow/Internal.hs        # NEW testable internals: endpoints/manager/delay injection.
test/Lib/GitHub/Auth/DeviceFlowSpec.hs            # NEW embedded WAI mock-server tests.
app/moog-github-device-flow-smoke.hs              # NEW live-boundary smoke harness.
```

`Lib.GitHub.Auth.DeviceFlow` exports only the issue-defined API. The internal module exists so the unit tests can point the state machine at the WAI mock server without changing the public `runDeviceFlow` signature. Sibling tickets must import the public module, not the internal one.

## State machine

The implementation should model the GitHub wire shape directly:

- Device-code request: `POST /login/device/code`, form-encoded `client_id` and space-joined `scope`, with `Accept: application/json`.
- Access-token request: `POST /login/oauth/access_token`, form-encoded `client_id`, `device_code`, and `grant_type=urn:ietf:params:oauth:grant-type:device_code`, with `Accept: application/json`.
- Success response: JSON object containing `access_token`.
- Pending response: JSON object containing `error = "authorization_pending"`.
- Slow-down response: JSON object containing `error = "slow_down"`.
- Terminal errors: `expired_token`, `access_denied`.

The internal entry point should accept endpoint URLs and a delay function so tests run fast and deterministically.

## Slices

### Slice 0 — Orchestration bootstrap

Owned by the ticket-orchestrator. Add `gate.sh`, spec, plan, and tasks; open the draft PR. No production or test behavior changes.

### Slice 1 — Public API + mock-server unit coverage

Add the device-flow module, internal test hook, cabal module/dependency declarations, and the hspec mock-server suite. This is the type-pinning slice. After the orchestrator accepts and pushes this commit, append the required `NOTE RELEASE: oauth-token-type-pinned ...` line to `/tmp/epic-109/moog-111/STATUS.md`.

RED: add mock-server tests covering pending success, slow-down success, expired, denied, malformed JSON, callback fields, and parameter client id. Tests fail because the module does not exist.

GREEN: implement the state machine with injectable internals and the public `runDeviceFlow` pointed at github.com.

Bisect-safe because the new module is additive and has complete focused tests.

### Slice 2 — Live-boundary smoke harness

Add a tiny Cabal executable that accepts `--client-id`, calls the public `runDeviceFlow`, prints the verification URI and user code, and on success prints only a token prefix of 4 characters plus total token length. It must not read the client id from an environment variable.

RED: add an executable-level smoke check or unit-level output-safety test proving the renderer never prints the full token.

GREEN: implement the harness and cabal executable stanza.

Bisect-safe because the executable is additive and does not alter library behavior.

### Slice 3 — Real OAuth App smoke proof

Use the OAuth App client id from #110 to run the live smoke manually through `cabal run moog-github-device-flow-smoke -- --client-id <id>`. Record the transcript artifact in `WIP.md` and the PR body, redacting the token except for first 4 characters and total length.

If the client id is not available when this slice begins, raise a parent Q-file and log `BLOCKED Q-NNN-...` in STATUS.md.

## Risks + mitigations

- **Testing a fixed github.com endpoint.** The public API intentionally has no endpoint parameter, so tests use an internal injectable entry point. Public consumers still see only the issue-defined function.
- **Long sleeps in unit tests.** The internal delay hook must be injectable; unit tests record requested delays without sleeping.
- **Token leakage.** The smoke harness must construct an explicit redacted string and never log `unOAuthToken` directly.
- **OAuth App dependency.** The live smoke depends on #110. Planning and implementation can proceed before the real client id exists; only Slice 3 blocks on it.

## Gate

`./gate.sh` runs:

```bash
git diff --check
nix build --quiet .#unit-tests --no-link
```

The live-boundary smoke is not run automatically by `gate.sh` because it needs an operator-approved OAuth App client id and browser approval. It is a named pre-ready proof in Slice 3.
