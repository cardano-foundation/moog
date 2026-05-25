# Implementation Plan: Agent File-backed Configuration

## Technical Context

- Language: Haskell
- Option parser: `opt-env-conf`
- Main affected parsers:
  - `Core.Options.tokenIdOption`
  - `Core.Types.MPFS`
  - `Oracle.Process.pollIntervalOption`
  - `User.Agent.Options`
  - `User.Agent.Process`
- Documentation:
  - `docs/ops/deployment.md`
  - `docs/user/secrets-management.md`

## Design

Add `conf` entries to existing settings instead of inventing a new config
loader. This keeps one precedence model and lets `--help` document the file
keys automatically.

The config keys are:

- `tokenId`
- `mpfsHost`
- `wait`
- `mpfsTimeoutSeconds`
- `pollIntervalSeconds`
- `minutes`
- `registry`
- `antithesisUser`
- `antithesisLaunchUrl`
- existing keys such as `githubPAT`, `agentEmail`, `agentEmailPassword`,
  `antithesisPassword`, `trustedRequesters`, `slackWebhook`, and wallet keys

`DOCKER_CONFIG` remains a container/runtime environment concern because the
Docker CLI reads it directly. Compose can still keep this stable by mounting
`/secrets/moog-agent/current/docker/config.json` at `/run/secrets/docker/config.json`
and setting `DOCKER_CONFIG=/run/secrets/docker`.

## Slices

### Slice 1: Parser support and tests

Add missing `conf` keys to the relevant parsers and unit tests covering YAML
parsing plus env-over-config precedence.

### Slice 2: Deployment docs

Update operator docs with a complete file-backed agent configuration and
current-directory rotation pattern.

## Verification

- `./gate.sh`
- Focused unit test during development:
  `cabal test unit-tests --test-show-details=direct --test-option=--match --test-option="agent file-backed configuration"`

