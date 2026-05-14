# Tasks — issue #92

Each top-level item is one commit. Sub-bullets all land in the same
commit so the slice stays bisect-safe.

## T1. feat: thread Antithesis launch URL through config

- `src/User/Agent/PushTest.hs`
  - Add `newtype LaunchUrl = LaunchUrl { unLaunchUrl :: String }`
    deriving `Show, Eq`. Export.
  - Add `launchUrl :: LaunchUrl` to `AntithesisAuth`.
  - In `renderPostToAntithesis`, use `unLaunchUrl launchUrl`
    instead of the literal URL.
- `src/User/Agent/Options.hs`
  - Add `launchUrlOption :: Parser LaunchUrl` using `setting`
    with `env "MOOG_ANTITHESIS_LAUNCH_URL"`, `conf
    "antithesisLaunchUrl"`, `long "launch-url"`, `metavar
    "LAUNCH_URL"`, `reader str`, `option`. No `value`.
  - Extend `antithesisAuthOption` to also parse the launch URL and
    populate the new field.
  - Re-export `LaunchUrl` from `User.Agent.PushTest` if needed for
    the parser (or import-only).
- `test/User/Agent/PushTestSpec.hs`
  - Update the auth value to `AntithesisAuth "user" "pass"
    (LaunchUrl "https://example.antithesis.com/api/v1/launch/x")`.
  - Update the expected args list to expect that URL instead of
    the old hardcoded one. The test now proves the URL is
    config-driven.
- **Bisect-safety**: the signature of `AntithesisAuth` changes
  atomically with its sole consumer and the test, so every
  intermediate build state remains green.

## T2. feat: configure Antithesis registry via MOOG_REGISTRY

- `src/User/Agent/Options.hs`
  - In `registryOption`, switch from `strOption` to `setting` so it
    can take both `long` and `env`.
  - Drop the `value "us-central1-docker.pkg.dev/..."` default.
  - Add `env "MOOG_REGISTRY"`.
- No test change: `PushTestSpec` constructs `Registry` directly.

## T3. docs: document multi-tenant Antithesis config

- `docs/user/secrets-management.md`
  - Add `antithesisLaunchUrl` to the agent `secrets.yaml` example
    and to the "All supported keys" table.
  - Add `anti_url[antithesisLaunchUrl]` node to the Mermaid graph
    with an `Agent -->` edge.
- `docs/ops/deployment.md`
  - Add `MOOG_ANTITHESIS_LAUNCH_URL` and `MOOG_REGISTRY` to the
    example compose env list.
  - Add both vars to the "Environment Variables Reference" table.
  - Update the registry text — there is no default any more.
- `docs/ops/agent-role.md`
  - Add `MOOG_ANTITHESIS_LAUNCH_URL` and `MOOG_REGISTRY` to the
    env-vars table.
- `CD/moog-agent/docker-compose.yaml`
  - Add commented `MOOG_ANTITHESIS_LAUNCH_URL` line under the
    `environment:` list (commented because the value is tenant-
    specific; deployer must uncomment + set).
  - Add `antithesisLaunchUrl` to the `secrets.yaml` key list
    comment at the bottom.
- `CHANGELOG.md` — not edited manually; release-please picks up the
  conventional-commit titles at release time.

## Gate

`nix develop --quiet -c just CI` between slice 1 and slice 2, and
again after slice 3.

## Out of scope

- Recipients list and email From header (issue defers these).
- Speckit init (PR #87 in flight; do not touch).
