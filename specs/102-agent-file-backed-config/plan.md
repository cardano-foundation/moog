# Implementation Plan: moog-agent File-backed Configuration

## Issue

cardano-foundation/moog#102

## Goal

Make one deployed `moog-agent` instance configurable from file-backed YAML
settings so operators can rotate old/new Antithesis configuration by changing
mounted files or a stable `current` symlink, without editing Docker Compose.

## Technical Context

- Language: Haskell
- Parser framework: `opt-env-conf`
- Config entry point: `withYamlConfig secretsFileOption`
- Existing agent process entry point: `User.Agent.Process.parseArgs`
- Existing shipped command: `moog-agent`
- Existing deployment docs: `docs/ops/deployment.md` and
  `docs/user/secrets-management.md`

## Proposed YAML Keys

The implementation should use explicit config keys that are visible in
`moog-agent --help` through `config:` documentation:

- `tokenId`
- `mpfsHost`
- `wait`
- `mpfsTimeoutSeconds`
- `pollIntervalSeconds`
- `minutes`
- `registry`
- `antithesisUser`
- `antithesisLaunchUrl`
- Existing keys already supported by the secrets parser:
  `githubPAT`, `agentEmail`, `agentEmailPassword`, `antithesisPassword`,
  `trustedRequesters`, `slackWebhook`, `walletPassphrase`, `mnemonics`, and
  `encryptedMnemonics`

`DOCKER_CONFIG` should remain a stable container runtime environment variable
because Docker reads it directly. Compose can still avoid per-rotation edits by
mounting `/secrets/moog-agent/current/docker/config.json` at
`/run/secrets/docker/config.json` and setting `DOCKER_CONFIG=/run/secrets/docker`.

## Design

Extend the existing `opt-env-conf` parsers with `conf` entries instead of
adding a second configuration loader. That preserves current parser behavior,
help generation, environment support, and CLI flag support.

Per-tenant values that currently have source-level defaults or hard-coded
values should become explicit configuration. In particular:

- Registry should be configurable through `--registry`, `MOOG_REGISTRY`, and
  `registry`.
- Antithesis launch URL should be configurable through `--launch-url`,
  `MOOG_ANTITHESIS_LAUNCH_URL`, and `antithesisLaunchUrl`, then threaded into
  the code path that renders the Antithesis launch request.
- Required tenant values should fail closed when absent instead of silently
  targeting the previous tenant.

Environment variables and CLI flags must remain supported. Precedence should
follow existing `opt-env-conf` semantics and must be covered by tests.

## Slice Breakdown

### Slice 1: Parser Config Keys And Tests

Add config-file keys to existing parser settings for token id, MPFS settings,
poll interval, result lookback minutes, registry, Antithesis username, and
wallet file path if needed for the process path. Add focused unit tests proving
YAML parsing and env-over-YAML precedence.

Owned paths for worker brief:

- `src/Core/Options.hs`
- `src/Core/Types/MPFS.hs`
- `src/Core/Types/Mnemonics/Options.hs`
- `src/Oracle/Process.hs`
- `src/User/Agent/Options.hs`
- `test/User/Agent/*`
- `moog.cabal` if a new test module is added

### Slice 2: Antithesis Launch URL Threading

Thread the Antithesis launch URL from parsed agent configuration into the
request rendering path instead of using the hard-coded Cardano tenant URL.
Keep existing command behavior where explicit CLI/env/config supplies the
value, and fail closed when no launch URL is supplied.

Owned paths for worker brief:

- `src/User/Agent/Options.hs`
- `src/User/Agent/PushTest.hs`
- `src/User/Agent/Process.hs` if the process record needs an additional field
- `test/User/Agent/PushTestSpec.hs`
- `test/User/Agent/*`

### Slice 3: Deployment Documentation

Document the complete file-backed agent YAML shape and the stable-path
rotation model. Keep Docker credentials as a mounted Docker config file, not
raw YAML.

Owned paths for worker brief:

- `docs/ops/deployment.md`
- `docs/user/secrets-management.md`
- `docs/ops/agent-role.md` if the environment/config table needs alignment
- `CD/moog-agent/docker-compose.yaml` only if the canonical example should
  switch to `/secrets/moog-agent/current/...`

## TDD And Verification

Each behavior slice should follow RED -> GREEN:

- RED: add focused unit coverage that fails on `origin/main`.
- GREEN: implement the parser/threading change.
- Gate: run the focused unit test first, then the branch gate.

Expected branch gate for worker briefs:

```bash
git diff --check
nix develop --quiet -c cabal build all --enable-tests
nix develop --quiet -c cabal test unit-tests --test-show-details=direct
nix develop --quiet -c cabal-fmt -c moog.cabal CI/rewrite-libs/rewrite-libs.cabal
nix develop --quiet -c fourmolu -m check src app test CI/rewrite-libs
nix develop --quiet -c hlint -c src app test CI/rewrite-libs
```

The full repository `just CI` includes integration/E2E flows that require
operator-provided secrets and wallet fixtures. If those are unavailable during
this PR, record the limitation in `WIP.md` and the PR body rather than claiming
full CI equivalence.

## Open Questions For Next Phase

- Should `registry` remain optional with the current default for requester-style
  ad hoc `push-test`, or should it become required everywhere to satisfy
  fail-closed tenant behavior?
- Should `antithesisLaunchUrl` be stored inside `AntithesisAuth` or a sibling
  deployment/tenant configuration record?
- Should the canonical compose example in `CD/moog-agent/docker-compose.yaml`
  move to `/secrets/moog-agent/current/...`, or should only the documentation
  show that operator rotation pattern?

## Out Of Scope

- Multi-tenant scheduling.
- Removing existing CLI flags or environment variables.
- Changing wallet, Docker registry, or Antithesis API formats.

