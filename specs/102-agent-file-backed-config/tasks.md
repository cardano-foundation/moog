# Tasks: moog-agent File-backed Configuration

## Issue

cardano-foundation/moog#102

## Acceptance Target

After all slices land, a deployed `moog-agent` can load its Antithesis/runtime
configuration from a YAML file selected through stable mounted paths, while
existing CLI flags and environment variables continue to work.

## Slice 1 — Parser Config Keys And Tests

Goal: extend the existing `opt-env-conf` parser surface so the agent runtime
settings have documented YAML keys, and prove YAML parsing plus precedence.

Independent proof:

```bash
nix develop --quiet -c cabal test unit-tests --test-show-details=direct --test-options='--match agent file-backed configuration'
```

Tasks:

- [X] T102-S1 Add failing unit coverage for `moog-agent` file-backed runtime configuration in `test/User/Agent/*`, using a temporary YAML file and temporary wallet JSON.
- [X] T102-S1 Add a failing precedence test proving an explicit environment value such as `MOOG_TOKEN_ID` overrides a YAML value in `test/User/Agent/*`.
- [X] T102-S1 Add `conf` keys for token id, MPFS host, wait behavior, MPFS timeout, poll interval, result lookback minutes, registry, Antithesis username, and wallet file path where needed in `src/Core/Options.hs`, `src/Core/Types/MPFS.hs`, `src/Core/Types/Mnemonics/Options.hs`, `src/Oracle/Process.hs`, and `src/User/Agent/Options.hs`.
- [X] T102-S1 Remove the source-level registry tenant default and add `MOOG_REGISTRY` as the explicit environment source for registry configuration.
- [X] T102-S1 Update `moog.cabal` only if the slice adds a new unit test module.
- [X] T102-S1 Run the focused unit test and the branch gate before committing.

## Slice 2 — Antithesis Launch URL Threading

Goal: remove the hard-coded Antithesis launch URL from the request path and
make it explicit file/env/CLI-backed configuration.

Independent proof:

```bash
nix develop --quiet -c cabal test unit-tests --test-show-details=direct --test-options='--match renderPostToAntithesis'
```

Tasks:

- [X] T102-S2 Add failing unit coverage showing the rendered Antithesis launch request uses the configured launch URL in `test/User/Agent/PushTestSpec.hs` or a focused neighboring test module.
- [X] T102-S2 Add `--launch-url`, `MOOG_ANTITHESIS_LAUNCH_URL`, and `antithesisLaunchUrl` support in `src/User/Agent/Options.hs`.
- [X] T102-S2 Store the launch URL with `AntithesisAuth` and thread it into the Antithesis request rendering path in `src/User/Agent/PushTest.hs` and any call sites that construct or pattern-match `AntithesisAuth`.
- [X] T102-S2 Ensure required tenant-specific launch URL configuration fails closed when absent rather than silently using the previous tenant.
- [X] T102-S2 Run the focused unit test and the branch gate before committing.

## Slice 3 — Deployment Documentation

Goal: document the complete operator-facing file-backed configuration and the
stable-path rotation pattern.

Independent proof:

```bash
git diff --check
```

Tasks:

- [ ] T102-S3 Document the complete agent YAML shape in `docs/user/secrets-management.md`, including newly file-backed runtime keys and existing secret keys.
- [ ] T102-S3 Document the stable `/secrets/moog-agent/current -> old|new` rotation pattern in `docs/ops/deployment.md`.
- [ ] T102-S3 Align the agent operations configuration table in `docs/ops/agent-role.md` if it would otherwise contradict the new file-backed configuration path.
- [ ] T102-S3 Update `CD/moog-agent/docker-compose.yaml` so the canonical compose example uses `/secrets/moog-agent/current/...` stable paths.
- [ ] T102-S3 Run the branch gate before committing.

## Branch Gate

Each worker slice must run the focused proof for the slice and then the branch
gate:

```bash
git diff --check
nix develop --quiet -c cabal build all --enable-tests
nix develop --quiet -c cabal test unit-tests --test-show-details=direct
nix develop --quiet -c cabal-fmt -c moog.cabal CI/rewrite-libs/rewrite-libs.cabal
nix develop --quiet -c fourmolu -m check src app test CI/rewrite-libs
nix develop --quiet -c hlint -c src app test CI/rewrite-libs
```

If integration/E2E credentials or wallet fixtures are unavailable, record that
limitation in `WIP.md` and in the PR body. Do not claim `just CI` equivalence
without running it.

## Dependency Order

1. Slice 1 must land before Slice 2 because launch URL parsing builds on the
   same parser/config conventions.
2. Slice 2 must land before Slice 3 so documentation reflects the final
   behavior rather than a planned API shape.
3. Slice 3 can be reviewed independently once Slices 1 and 2 define the exact
   key names and compose behavior.

## Parallelization Notes

Do not run Slices 1 and 2 in parallel: both may touch
`src/User/Agent/Options.hs`, tests under `test/User/Agent/*`, and possibly
`moog.cabal`.

Slice 3 may be drafted in parallel only after the exact key names from Slices 1
and 2 are stable. If documentation touches `CD/moog-agent/docker-compose.yaml`,
serialize it with implementation review because it changes the deployable
example.
