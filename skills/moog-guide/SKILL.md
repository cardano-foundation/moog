---
name: moog-guide
description: Repository guide for cardano-foundation/moog — administering Antithesis test execution through Cardano. Load when working in this repo or answering questions about it - the moog CLI and its subcommands (wallet, requester, agent, oracle, facts, token, retract, antithesis), the moog-oracle / moog-agent / moog-antithesis-proxy services, MOOG_* environment variables (MOOG_MPFS_HOST, MOOG_TOKEN_ID, MOOG_WALLET_FILE, MOOG_GITHUB_PAT, MOOG_ANTITHESIS_LAUNCH_URL), the MPFS service and on-chain facts, test-run phases (pending/accepted/finished/rejected), Antithesis run reconciliation, building with cabal/nix/just, release artifacts (musl tarballs, AppImage, deb, rpm), or the docs site under docs/.
---

# Moog repository guide

## Repository map

| Path | Purpose |
|---|---|
| `app/` | Executable entry points: `Main.hs` (CLI), `moog-oracle.hs`, `moog-agent.hs`, `moog-antithesis-proxy.hs`, `moog-mpfs-v2-canary.hs`, GitHub auth smoke tools |
| `src/Cli.hs`, `src/Options.hs` | CLI command GADTs and the opt-env-conf root parser |
| `src/Core/` | Shared types (wallet, facts, tx, mnemonics) and common options |
| `src/Wallet/`, `src/User/Requester/`, `src/User/Agent/`, `src/Oracle/` | Per-role commands: wallet management, requester requests, agent automation, oracle validation and token updates |
| `src/Oracle/Validate/` | Request validation rules (users, roles, whitelists, test-run transitions) |
| `src/User/Agent/Antithesis/` | Agent-side Antithesis API client and pure reconciliation rules (`State.hs`, `Plan.hs`) |
| `src/User/Antithesis/` | `moog antithesis` subcommands (read runs through the proxy) |
| `src/Proxy/Antithesis/` | The antithesis proxy server: API, auth middleware, cache, audit |
| `src/MPFS/` | MPFS HTTP API client |
| `src/Lib/` | GitHub access, SSH keys, JSON canonicalization, crypto helpers |
| `test/`, `test-integration/`, `test-E2E/`, `test-lib/` | Unit, integration and E2E suites plus shared test helpers |
| `docs/`, `mkdocs.yml` | MkDocs Material documentation site (user, architecture, ops, dev) |
| `nix/`, `flake.nix`, `justfile` | Build system: haskell.nix project, docker images, dev shell, recipes |
| `CD/` | Production docker-compose files for oracle and agent |
| `CI/`, `.github/workflows/` | CI tooling and workflows (unit, integration, E2E, releases, docs) |
| `scripts/` | Operator helpers (`recent-runs.sh`, `wait-for-test.sh`) |

## Build, test, run

```bash
nix develop              # dev shell: GHC 9.12, cabal, HLS, fourmolu, hlint, mkdocs
just build               # cabal build all --enable-tests
just unit                # unit tests (or: nix run .#unit-tests)
just format              # fourmolu + cabal-fmt + nixfmt
just hlint               # lint
nix build .#moog         # build the CLI with nix (also: .#moog-oracle, .#moog-agent, .#moog-antithesis-proxy)
mkdocs serve             # preview the docs site locally (inside nix develop)
```

Integration and E2E tests (`just integration`, `just E2E`) need Docker, a
wallet file in `tmp/test.json` and the `MOOG_GITHUB_PAT` /
`MOOG_SSH_PASSWORD` environment variables; read the recipes in `justfile`
before running them.

## Navigating the code

- The CLI parser starts at `src/Options.hs` (`optionsParser`); each
  subcommand group has its own `Options.hs` (parser) and `Cli.hs`
  (interpreter) under `src/Wallet/`, `src/User/Requester/`,
  `src/User/Agent/`, `src/Oracle/`. Options use opt-env-conf, so most
  settings exist as flag + `MOOG_*` env var + YAML config key
  simultaneously.
- The oracle service loop is `src/Oracle/Process.hs`; validation logic
  lives in `src/Oracle/Validate/Requests/`.
- The agent service loop is `src/User/Agent/Process.hs` (`pollOnce`); the
  pure decision rules that match Antithesis API runs to on-chain test-runs
  and derive outcomes are in `src/User/Agent/Antithesis/State.hs` and
  `Plan.hs`.
- The proxy routes are declared in `src/Proxy/Antithesis/Api.hs` and served
  in `Server.hs`; GitHub team auth is `Middleware/Auth.hs`.
- On-chain fact types (test-run phases pending/accepted/finished/rejected)
  are in `src/User/Types.hs` and `src/Core/Types/Fact.hs`.

## Using moog

The CLI needs `MOOG_MPFS_HOST` (production: `https://mpfs.plutimus.com`)
and `MOOG_TOKEN_ID` for anything on-chain; see
`docs/user/configuration.md` for the production token id.

```bash
moog wallet create --wallet wallet.json   # create a wallet
moog wallet info --wallet wallet.json     # address + public key hash
moog facts users                          # registered users (read-only)
moog facts test-runs pending              # pending test-runs
moog token                                # token state + pending requests
moog requester create-test --platform github --username USER \
    --repository ORG/REPO --directory DIR --commit SHA --try N --duration HOURS
moog agent query                          # test-runs by phase, agent view
moog antithesis runs --limit 10           # Antithesis runs via the proxy
```

Note: `moog oracle token boot` / `token end` were removed in v0.5.1.3
(#144); only `moog oracle token update` and `moog oracle config set`
remain. The MPFS v2 cutover happens on the `moog-v2` branch.

## Answering questions

- "What is moog / how does it work?" — README **What is this** +
  **Architecture**; deeper: `docs/ops/architecture.md` and
  `docs/ops/protocol.md`.
- "How do I install / set up?" — `docs/user/installation.md` and
  `docs/user/configuration.md` (wallet, env vars, production token).
- "How do I request a test run?" — `docs/user/usage.md`; CI integration in
  `docs/user/ci.md`.
- "How do I run the oracle/agent in production?" — `docs/ops/deployment.md`,
  `docs/ops/oracle-role.md`, `docs/ops/agent-role.md`,
  `docs/ops/agent-oracle-config-secrets.md`; live-incident procedures in
  `skills/antithesis-moog/SKILL.md`.
- "Why is a test-run stuck / how are results derived?" — the
  reconciliation rules in `src/User/Agent/Antithesis/State.hs` are the
  source of truth; operator-facing summary in `docs/ops/agent-role.md` and
  `docs/ops/troubleshooting.md`.
- "What changed in release X?" — `CHANGELOG.md`.
- The docs site is published at
  <https://cardano-foundation.github.io/moog/>.
