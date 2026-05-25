# Feature Specification: moog-agent File-backed Configuration

## Issue

cardano-foundation/moog#102

## P1 User Story

As a `MOOG operator`, I switch a deployed `moog-agent` between old and new
Antithesis configuration files and observe the agent use the selected
configuration without editing Docker Compose environment variables.

## Command-Recovery

Yes. This touches the shipped `moog-agent` process startup and configuration
surface. The delivered proof must exercise the operator-facing command/config
path, not only internal parser helpers.

## Problem Statement

`moog-agent` already loads a YAML file through `--secrets-file` /
`MOOG_SECRETS_FILE`, and several sensitive values are already file-backed
through that path. The deployed agent still requires some Antithesis/runtime
settings to live in Docker Compose environment variables or source defaults.

That split makes operational rotation awkward. An operator should be able to
keep Compose pointed at stable mounted paths and switch from an old config set
to a new config set by changing files or a `current` symlink, without editing
the Compose file for each rotation.

## Current Findings

- `secrets.yaml` already supports keys such as `githubPAT`, `agentEmail`,
  `agentEmailPassword`, `antithesisPassword`, `trustedRequesters`,
  `slackWebhook`, and wallet passphrase/material where applicable.
- `moog-agent` still needs additional deploy-time settings outside YAML,
  including Antithesis username, registry, launch URL, MPFS host, token id,
  wait behavior, poll interval, result lookback minutes, and MPFS timeout.
- `DOCKER_CONFIG` is a Docker CLI/runtime concern. It can remain an environment
  variable if Compose points it at a stable mounted directory such as
  `/run/secrets/docker`.
- Related issue #92 covers multiple Antithesis tenants. This issue is narrower:
  one deployed agent instance should be fully configurable from file-backed
  settings for rotation.

## Functional Requirements

- FR-001: `moog-agent --help` documents config-file keys for all agent
  Antithesis/runtime settings needed to launch and report tests, including
  Antithesis username, registry, launch URL, credentials, email settings,
  requester trust configuration, MPFS host, token id, wait/poll behavior,
  result lookback minutes, and MPFS timeout.
- FR-002: `moog-agent --secrets-file /path/to/agent.yaml` accepts the above
  values from YAML without requiring Compose to encode tenant-specific or
  rotation-specific values.
- FR-003: Existing environment variables and CLI flags continue to work.
- FR-004: Precedence is documented and tested. Explicit CLI/env values must
  override YAML values according to the existing option parser semantics.
- FR-005: Deployment documentation includes a complete file-backed agent
  configuration example.
- FR-006: Deployment documentation includes a stable-path rotation pattern such
  as `/secrets/moog-agent/current -> old|new` without editing Compose.
- FR-007: The implementation fails closed when a required per-tenant setting is
  absent instead of silently using the wrong tenant.

## Acceptance Criteria

- [ ] `moog-agent --help` includes `config:` entries for the newly file-backed
      settings.
- [ ] A parser or command-level test proves `moog-agent --secrets-file` can
      load the required file-backed settings.
- [ ] A precedence test proves an explicit CLI/env value overrides a YAML value.
- [ ] Documentation shows the complete YAML shape for the agent.
- [ ] Documentation shows the stable Compose mount and `current` symlink
      rotation model.

## Assumptions

- The existing `opt-env-conf` YAML integration remains the single source of
  parser behavior; this feature should extend existing parsers rather than add
  a separate configuration loader.
- Wallet files and Docker registry config files may remain separate mounted
  files. The goal is stable file-backed deployment configuration, not merging
  every secret into one YAML document.
- The final implementation may choose exact YAML key names during planning, but
  they should be explicit, documented, and visible in `--help`.

## Non-goals

- Do not implement full multi-tenant scheduling or tenant selection beyond
  enabling one agent instance to load its complete configuration from files.
- Do not remove existing environment variables or CLI options.
- Do not change wallet formats.
- Do not change Antithesis API semantics.
- Do not change Docker registry authentication semantics.
- Do not require operators to store raw Docker credentials inside the YAML file.

