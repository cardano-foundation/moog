# Changelog for moog-cli

### v0.5.1.5 - 2026-06-11

#### Fixed

- **The agent launch is idempotent and duplicate Antithesis runs drain
  instead of stalling.** A launched-but-not-yet-observed test-run is
  tracked with an in-process marker so a slow-to-appear run is not
  launched twice, and when multiple API runs match the same on-chain
  test-run the agent prefers the completed run as the canonical one and
  drains the duplicates. (#175)
- **The finished outcome is derived from run properties, not only run
  status.** A `completed` Antithesis run with failing test properties
  now finishes as a failure; Antithesis-platform/coverage properties are
  excluded from that check so platform noise cannot fail a run. (#190)
- **The dev shell solves again on GHC 9.12.3** by allowing a newer
  `base` for `cabal-fmt`. (#191)

### v0.5.1.4 - 2026-06-09

#### Fixed

- **Agent no longer OOM-loops on the Antithesis runs pagination.** Antithesis
  renamed the `GET /api/v0/runs` pagination query parameter from `cursor` to
  `after` under the same `v0`. The Servant-derived client still sent `cursor=`,
  which the upstream silently ignores — re-serving page 1 with an unchanged
  `next_cursor`. Once a tenant exceeded one page (100 runs), `listAllRuns`
  paginated forever, accumulating duplicates until the `moog-agent` process was
  OOM-killed and stopped launching test runs. The client now sends `after=`, and
  `listAllRuns` additionally bails when the cursor fails to advance so a future
  silent upstream change degrades gracefully instead of exhausting host memory.

### v0.5.1.3 - 2026-06-01

#### Fixed

- **Removed the broken oracle `token boot` / `token end` commands.**
  These were migrated to the new facts-only MPFS endpoints (`GET
  /status`, `POST /facts/boot`, `POST /facts/end`), which the production
  MPFS server (`https://mpfs.plutimus.com`) does not serve — every
  invocation returned 404. The operator CLI no longer exposes them, so
  the release advertises only operations that work against the
  production server. The existing moog token and all other oracle,
  requester, and agent operations are unaffected. The facts-only cutover
  for the whole API continues on a dedicated `moog-v2` branch. (#144)

### v0.5.1.2 - 2026-05-30

#### Fixed

- **Terminal Antithesis runs without a triage report now finish
  on-chain instead of waiting forever.** `moog-agent` treated any API
  run missing `links.triage_report` as still running, so a terminal
  `incomplete`/`cancelled` run left its on-chain test-run stuck in
  `accepted` indefinitely. The agent now finishes such failure-class
  runs as `OutcomeFailure` with a deterministic synthetic URL
  `antithesis://runs/<run_id>/no-triage-report`; `completed` without a
  report stays conservative (it keeps waiting, since success must not be
  attested without the report link). Agent logs no longer claim "still
  running" for terminal runs. (#138)

### v0.5.1.1 - 2026-05-30

#### Fixed

- **`moog antithesis` JSON output is now valid JSON.** The JSON
  subcommands (`runs`, `run`, `properties`) routed through the
  canonical-JSON renderer, which did not escape control characters
  inside string values. Any upstream payload containing embedded
  LF/CR (e.g. multi-line property descriptions) produced output that
  `jq` rejected. The CLI now writes responses via `Aeson.encode`
  (which always escapes control chars). (#136)
- **Streaming subcommands no longer append a trailing `null`.**
  `events`, `logs`, and `build-logs` previously printed `null` after
  the NDJSON stream, which broke `jq -s` consumers. The CLI now
  exits cleanly after the byte stream ends. (#136)

### v0.5.1.0 - 2026-05-29

#### Added

- **`moog-antithesis-proxy` daemon**: a GitHub-team-gated HTTP proxy at
  `antithesis-proxy.plutimus.com` that lets `pragma-org/antithesis-access`
  members read the Antithesis tenant API without holding the tenant API
  key. Auth: GitHub OAuth device-flow tokens → `whoami` + team membership
  check → Bearer-keyed forward to upstream.
- **`moog antithesis …` CLI subcommand group** on the moog binary,
  derived from the same Servant `AntithesisProxyAPI` the proxy serves:
  - `moog antithesis runs [--limit N] [--cursor S]` — paginated runs list
  - `moog antithesis run --run-id RUN_ID` — single-run detail
  - `moog antithesis properties --run-id RUN_ID` — property pass/fail
    (404 for in-progress runs)
  - `moog antithesis events --run-id RUN_ID [--q QUERY]` — NDJSON event
    search streamed verbatim to stdout
  - `moog antithesis logs --run-id RUN_ID [--input-hash …] [--vtime …]`
    — NDJSON logs at a moment, streamed
  - `moog antithesis build-logs --run-id RUN_ID` — NDJSON build log
    stream
- Proxied `GET /api/v0/openapi.json` so team members can read the
  Antithesis OpenAPI spec via their GitHub OAuth token (no Antithesis API
  key required).
- First-time `moog antithesis …` invocation runs the GitHub OAuth
  device flow against the `lambdasistemi`-owned `moog` OAuth App
  (client ID `Ov23liVVFVtdBez1QDxq`) and caches the token at
  `~/.config/moog/github-oauth.json` (mode 0600).
- `docs/antithesis-proxy.md` — full deployment + endpoint + CLI runbook.

#### Operations

- New compose service `moog-antithesis-proxy` on plutimus, fronted by
  Traefik (`certresolver=le`), reachable at
  `https://antithesis-proxy.plutimus.com`.
- New secret `/secrets/moog-antithesis-proxy/{new,old}/antithesis-api-key`
  carrying the Antithesis API key; consumed by the proxy via
  `MOOG_ANTITHESIS_API_KEY_FILE`.

#### CI

- `Build without nix` workflow made dispatch-only (was flaky on PRs).
- `MPFS v2 Canary` workflow made dispatch-only (was flaky on PRs).

---

### v0.5.0.0 - 2026-02-24

#### Breaking Changes

- **Duration wire format**: Duration fields (`maxDuration`, `minDuration`, `duration`) are now serialized as `{"hours": N}` or `{"minutes": N}` instead of plain integers. Older moog versions (< 0.5.0.0) cannot parse facts written by this version.
- **Configuration**: Oracle config `--min-test-duration` and `--max-test-duration` now accept minutes instead of hours.

#### Added

- Sub-hour test run support via new `Duration` type (hours and minutes granularity).
- Protocol version field in oracle config fact for forward-compatible version checking.
- MkDocs documentation (migrated from Docusaurus).
- E2E and integration tests run via Nix in CI.

#### Fixed

- Handle Antithesis error/failure emails in outcome parser.
- Configuration update bug.

---

### v0.4.1.2 - 2025-12-02

#### Added

- Support for specifying timeouts against MPFS services in `moog-agent` and `moog-oracle` defaulting at 120 seconds but configurable via `MOOG_MPFS_TIMEOUT_SECONDS` environment variable.

---

### v0.4.1.1 - 2025-11-04

#### Changes

- Changed agent option from `--days` to `--minutes` to specify the time window for checking test results.

---

### v0.4.1.0 - 2025-10-29

### Added

- Support for creating tests with faults injector disabled.

---

### v0.4.0.0 - 2025-10-28

#### Added
- Support for `moog` exe as a docker image, light and full versions.
- Slot in facts. Each fact now comes with the slot of the last transaction that modified it.

---

### v0.3.3.1 - 2025-10-28

#### Bug Fixes
- Fixed parsing of outcome field in old test runs.

---

### v0.3.3.0 - 2025-10-27

#### Added
- `outcome` field of test runs which is either `"success"` `"failure"` or `"unknown"`

---
### v0.3.2.0 - 2025-10-20
#### Bug Fixes
- Fixed oracle process image to contain the docker command
- Removed json nulls as value in commands outputs

---

### v0.3.1.0 - 2025-10-02
#### Added
- Validation of Docker Compose configuration after checking for the file's existence.

#### Bug Fixes
- `moog --version` now correctly shows `0.3.1.0`.

---

### v0.3.0.0 - 2025-09-30
#### Changes
- **SSH authentication is now deprecated:**
    - Users can still use existing SSH-based registrations to create test-runs (up to but not including v0.5.0.0).
    - SSH-based new registrations are no longer supported.
    - SSH-based unregistrations are not validated (anyone can unregister any SSH-registered user).

#### Added
- Support for registrations (or re-registration of old users) via `vkey` publishing.

---

### v0.2.2.0 - 2025-09-25
#### Added
- Support for CODEOWNERS file in `.github` and `docs` directories.

---

### v0.2.1.0 - 2025-09-20
#### Changes
- GitHub usernames and repository names are now case-insensitive, aligning with GitHub's naming conventions.
- Duplicate registrations differing only by letter case are no longer allowed.

---

### v0.2.0.3 - 2025-09-15
#### Changes
- Removed support for multiple SSH keys for the same GitHub user.
- Registrations with multiple SSH keys are no longer allowed.

---

### v0.2.0.1 - 2025-09-10
#### Bug Fixes
- Fixed a bug that allowed unregistering valid users and roles.

---

### v0.2.0.0 - 2025-09-05
#### Added
- `moog agent` executable to automate the agent role.
- Wallet encryption/decryption for all roles, available at `moog wallet` command.
- `moog agent push-test` command for the agent to start a test.
- `moog agent report-results` now encrypts the URL with results using the requester's SSH public key.
- `moog facts test-runs done` now decrypts the result URL when the requester SSH key is accessible.
- `moog agent collect-email-results` command that scans the agent email via IMAP to collect results.
- More filters in `moog facts test-runs` command: `--whose` and `--test-run-id`.
- Requesters can now provide any test assets, as long as they include a working `docker-compose.yml` file.

#### Bugs
- Interactive secret input switch is still available but may not appear in some help messages or may appear twice (e.g., `--ask-ssh-password`).

---

### v0.1.1.0 - 2025-08-25
#### Added
- `moog` executable.
