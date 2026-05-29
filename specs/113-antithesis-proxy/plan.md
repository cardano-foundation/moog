# Implementation Plan: moog-antithesis-proxy

## Technical Shape

The proxy is implemented as a WAI application in the `moog` library and
started by a new `moog-antithesis-proxy` executable. The application uses
the public #112 APIs:

- `Lib.GitHub.Auth.Identify.whoami`
- `Lib.GitHub.Auth.TeamCheck.checkTeamMembership`

The auth middleware is parameterized by those functions so tests can
exercise every membership result without live GitHub calls. The
production constructor wires the real functions.

The Antithesis upstream client uses `http-client`. Tests use a local WAI
`warp` server as the upstream boundary.

## Slice Breakdown

### Slice 1: Config, Cache, Audit, Cabal Scaffold

Add environment parsing, the membership cache, audit event rendering,
the executable stanza, and a skeletal `main` that can parse settings.
This slice proves defaults, env overrides, secret-file loading, cache
hit/miss/expiry, and audit sanitization.

### Slice 2: Auth Middleware

Add `Proxy.Antithesis.Middleware.Auth`, including bearer-token parsing,
GitHub identity/team injection points, 60-second cache use, WAI vault
login attachment, and required `401`/`403` response mapping.

### Slice 3: Runs Handler

Add `Proxy.Antithesis.Handler.Runs` forwarding
`GET /api/v1/runs`, preserving query strings, attaching Antithesis basic
auth, passing through safe response headers and body, sanitizing
upstream 5xx responses, and reporting upstream status to audit context.

### Slice 4: Application Assembly and Readiness

Assemble the WAI app with `GET /healthz`, `GET /readyz`,
`GET /api/v1/runs`, and `404` fallback. Add readiness checks against the
Antithesis runs endpoint and GitHub API. Wire `app/moog-antithesis-proxy.hs`
to Warp settings.

### Slice 5: Final Verification and PR Metadata

Run the full gate, build the executable target, smoke `/healthz`, update
the PR body, and push the final branch state.

## Verification

Each implementation slice follows RED -> GREEN:

1. Add focused tests and run the focused command to observe failure.
2. Add minimal implementation.
3. Run the focused command and `./gate.sh`.
4. Commit one bisect-safe slice with a `Tasks:` trailer.
5. Push only after local verification.
