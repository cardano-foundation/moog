# Tasks: moog-antithesis-proxy

## Slice 1 - Config, Cache, Audit, Cabal Scaffold

- [X] T113-S1 Add failing tests for settings parsing, membership cache,
  and audit rendering.
- [X] T113-S1 Implement `Proxy.Antithesis.Config`,
  `Proxy.Antithesis.Cache`, and `Proxy.Antithesis.Audit`.
- [X] T113-S1 Add cabal exposure and a skeletal
  `moog-antithesis-proxy` executable.
- [X] T113-S1 Run focused tests and `./gate.sh`, then commit.

## Slice 2 - Auth Middleware

- [ ] T113-S2 Add failing auth middleware tests for missing/malformed
  token, invalid token, pending, not-member, SSO-required, other error,
  and success.
- [ ] T113-S2 Implement bearer auth, membership caching, vault login
  attachment, and failure response mapping.
- [ ] T113-S2 Run focused tests and `./gate.sh`, then commit.

## Slice 3 - Runs Handler

- [ ] T113-S3 Add failing runs handler tests with a WAI mock upstream.
- [ ] T113-S3 Implement query-preserving forwarding, Antithesis basic
  auth, pass-through headers, streamed response handling, and sanitized
  upstream 5xx responses.
- [ ] T113-S3 Run focused tests and `./gate.sh`, then commit.

## Slice 4 - Application Assembly and Readiness

- [ ] T113-S4 Add failing application tests for `/healthz`, `/readyz`,
  `GET /api/v1/runs`, and unknown routes.
- [ ] T113-S4 Assemble the WAI app, readiness checks, and Warp
  executable entrypoint.
- [ ] T113-S4 Run focused tests, executable build, and `./gate.sh`, then
  commit.

## Slice 5 - Final Verification and PR Metadata

- [ ] T113-S5 Run final `./gate.sh`, build/smoke
  `moog-antithesis-proxy`, update PR metadata, and mark the ticket
  complete.
