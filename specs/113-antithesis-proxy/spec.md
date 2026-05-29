# Feature Specification: moog-antithesis-proxy

## User Story

As a Moog operator, I need a long-running HTTP proxy in front of the
Antithesis tenant API so that access to run data is gated by GitHub
team membership and the tenant basic-auth secret stays server-side.

## Functional Requirements

- `moog-antithesis-proxy` serves `GET /healthz` without auth and returns
  `200 ok`.
- `GET /readyz` returns `200 ready` only when the configured Antithesis
  `/api/v1/runs` endpoint and GitHub API are reachable; otherwise it
  returns `503`.
- `GET /api/v1/runs` requires `Authorization: Bearer <token>`.
- Missing or malformed bearer tokens return `401` with
  `WWW-Authenticate: Bearer realm="moog-antithesis-proxy"`.
- The proxy identifies the token owner with `whoami`, then checks
  membership in `MOOG_PROXY_AUTHORIZED_ORG` /
  `MOOG_PROXY_AUTHORIZED_TEAM`.
- Invalid tokens return `401`; pending invitations and non-members return
  `403`; SSO-required responses return `403` with `X-Moog-SSO-Url`.
- Authorized requests forward to
  `$MOOG_ANTITHESIS_URL/api/v1/runs`, preserving the query string and
  sending server-held Antithesis basic auth from
  `MOOG_ANTITHESIS_USER` plus `MOOG_ANTITHESIS_PASSWORD_FILE`.
- The runs response passes through upstream status, body, content-type,
  and content-length. Upstream 5xx client responses are sanitized.
- Every request emits one JSON audit line without tokens or upstream
  bodies, including timestamp, login, path, response status, optional
  upstream status, latency, and request id.
- Unknown endpoints return `404`.

## Configuration

- `MOOG_PROXY_BIND_ADDR`, default `0.0.0.0`
- `MOOG_PROXY_BIND_PORT`, default `8080`
- `MOOG_ANTITHESIS_URL`, default
  `https://amaru-cardano.antithesis.com`
- `MOOG_ANTITHESIS_USER`, default `pragma`
- `MOOG_ANTITHESIS_PASSWORD_FILE`, default
  `/run/secrets/antithesis-password`
- `MOOG_PROXY_AUTHORIZED_ORG`, default `pragma-org`
- `MOOG_PROXY_AUTHORIZED_TEAM`, default `antithesis-access`
- `MOOG_PROXY_MEMBERSHIP_TTL_SEC`, default `60`
- `MOOG_PROXY_LOG_LEVEL`, default `info`

## Success Criteria

- Unit tests cover auth middleware success and the required failure
  shapes.
- Unit tests cover membership cache TTL behavior without storing raw
  tokens.
- Unit tests cover runs proxying through a mock WAI upstream.
- Integration-style unit tests cover `/readyz` success and failure
  against mock endpoints.
- `moog-antithesis-proxy` cabal target builds.
- `./gate.sh` passes before every pushed slice.
