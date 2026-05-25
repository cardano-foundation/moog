# Feature Specification: Agent File-backed Configuration

## P1 User Story

As a `MOOG operator`, I switch a deployed `moog-agent` between old and new
Antithesis configuration files and observe the agent use the selected
configuration without editing Docker Compose environment variables.

## Scope

`moog-agent` already loads a YAML secrets/config file through
`--secrets-file` / `MOOG_SECRETS_FILE`, but several runtime settings are only
available through environment variables or CLI options. This feature makes the
agent deployment configuration file-backed wherever the parser already has a
natural config surface.

## Functional Requirements

- FR-001: The agent process accepts config-file keys for token id, MPFS host,
  MPFS wait behavior, MPFS timeout, polling interval, result email lookback
  minutes, registry, Antithesis username, Antithesis launch URL, Antithesis
  password, email credentials, GitHub PAT, requester trust list, and optional
  Slack webhook.
- FR-002: Existing CLI flags and environment variables continue to parse.
- FR-003: Explicit CLI/env configuration keeps precedence over YAML
  configuration.
- FR-004: `moog-agent --help` documents the new YAML config keys via
  `config:` entries.
- FR-005: Deployment documentation shows a stable compose mount that points at
  a switchable `/secrets/moog-agent/current` directory or symlink.

## Success Criteria

- A unit test proves the new YAML keys parse for the agent option parsers.
- A unit test proves an environment value overrides a YAML value for at least
  one newly file-backed setting.
- The docs contain a complete file-backed agent config example and a rotation
  pattern.

## Non-goals

- Do not implement full multi-tenant scheduling or tenant selection.
- Do not remove existing environment variables or CLI options.
- Do not change wallet formats, Antithesis APIs, or Docker registry
  authentication semantics.
- Do not require raw Docker credentials inside the YAML file.

