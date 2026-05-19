# Spec — Repair libsodium fork install in no-Nix build

- **Issue:** [#97](https://github.com/cardano-foundation/moog/issues/97)
- **Phase:** Specs
- **Command-recovery:** No. This ticket is CI-only; MOOG ships no
  operator command that builds libsodium.
- **Branch:** `fix/libsodium-no-nix-build`

## P1 user story

As a MOOG maintainer, I run the `Build moog without nix` workflow and
observe the Cardano-compatible `libsodium` fork install successfully on
a clean GitHub-hosted `ubuntu-latest` runner before Cabal dependency
resolution starts.

This is the paramount story. Until it holds, `Build moog without nix`
stays non-required (per the issue's non-goals) but cannot be used to
validate PRs that touch the no-Nix path.

## Context

`Build moog without nix` is defined in
`.github/workflows/build-no-nix.yaml`. The currently broken step is
`Install libsodium fork`, which:

1. Derives the libsodium fork revision from `iohk-nix`'s
   `flake.lock` (resolved via `cardano-node` `10.5.1`).
2. Clones `https://github.com/intersectmbo/libsodium`.
3. Checks out the resolved revision.
4. Runs `./autogen.sh`, `./configure --prefix=$HOME/.local`, `make`,
   `make install`.

The current failure on a clean runner:

```
Downloading config.guess and config.sub...
Done.
configure: error: cannot run /bin/bash ./build-aux/config.sub
```

Reference failing run: <https://github.com/cardano-foundation/moog/actions/runs/26026528215/job/76501180598>.

## Acceptance scenarios

### S1 — Cache miss on a fresh runner

**Given** a `Build moog without nix` run on a clean GitHub-hosted
`ubuntu-latest` runner where the `~/.local` cache key has never been
populated,
**When** the `Install libsodium fork` step executes,
**Then** the step completes successfully and produces
`$HOME/.local/lib/libsodium.so*`, `$HOME/.local/include/sodium.h`,
and `$HOME/.local/lib/pkgconfig/libsodium.pc`.

### S2 — Subsequent Cabal build can see sodium

**Given** S1 has succeeded,
**When** the later `Cabal build all` step exports
`PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig`,
**Then** `pkg-config --modversion libsodium` resolves and `cabal
build all` reaches dependency resolution and component compilation
without a sodium-related linker or pkg-config error.

### S3 — End-to-end job goes green on a cache miss

**Given** S1 and S2 hold,
**When** `Build moog without nix` is dispatched (manually or by a PR)
with all caches missing,
**Then** the job reaches `cabal build all` and the run URL is
attachable as proof on this PR.

### S4 — Cache hit still works

**Given** a prior successful run has populated the `~/.local` cache,
**When** the workflow is rerun and `libs-cache` reports a hit,
**Then** the `Install libsodium fork` step is skipped (its existing
`if: steps.libs-cache.outputs.cache-hit != 'true'` guard) and the
subsequent steps continue to find sodium via `PKG_CONFIG_PATH`.

### S5 — Fork pin remains derived from `iohk-nix`

**Given** a `Build moog without nix` run on `main`,
**When** the workflow resolves `IOHKNIX_VERSION` and then
`SODIUM_VERSION` from upstream flake locks,
**Then** the installed libsodium matches the same revision that the
pinned Cardano dependency stack expects, OR the spec/plan documents
an equivalent supported source pin and the workflow uses it
consistently.

## Functional requirements

- **FR1.** `Install libsodium fork` must install a Cardano-compatible
  libsodium build under `$HOME/.local` (libraries, headers, pkg-config
  file) on a clean `ubuntu-latest` runner.
- **FR2.** The fix must not depend on the `~/.local` cache for
  correctness; cache is allowed only as a speed optimization for cache
  hits (S4).
- **FR3.** The installed library version must remain the Cardano fork
  revision derived from `iohk-nix`, or an equivalent supported source
  documented in the plan and used consistently by the workflow.
- **FR4.** `PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig` and
  `LD_LIBRARY_PATH=$HOME/.local/lib` continue to surface sodium to the
  later `Cabal build all` step exactly as today.
- **FR5.** No change to the Nix build path, `flake.nix`, `nix/*.nix`,
  `haskell.nix` configuration, or `cabal.project` pins.
- **FR6.** No change to `Build moog without nix`'s required-check
  status in this PR; flipping it back to required is a follow-up the
  issue defers.

## Acceptance criteria (from issue #97)

- [ ] `Install libsodium fork` succeeds on a clean GitHub-hosted
  `ubuntu-latest` runner without relying on a stale `~/.local` cache.
- [ ] The workflow installs the same Cardano-compatible `libsodium`
  fork/revision expected by the pinned Cardano dependency stack, or
  documents and implements the equivalent supported source.
- [ ] `PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig` exposes the
  installed sodium package to the later Cabal build step.
- [ ] `Build moog without nix` reaches `cabal build all` on a cache
  miss.
- [ ] A PR proving the fix includes a workflow run URL where `Build
  moog without nix` passes.
- [ ] Once fixed, reconsider whether `Build moog without nix` should
  become a required status check again (tracked as follow-up, not in
  scope here).

## Non-goals

- Do not change the Nix build path or `haskell.nix` project
  configuration.
- Do not migrate MOOG away from the Cardano libsodium fork.
- Do not make `Build moog without nix` a required status check before
  the workflow is green.
- Do not broaden this ticket to unrelated no-Nix failures unless they
  block proof after the libsodium install is fixed.
- Do not fix pre-existing `actionlint -shellcheck` info-level findings
  on `build-no-nix.yaml`; they pre-date this ticket and are explicitly
  out of scope for the libsodium install repair.

## Exclusions / out of scope

- The Nix-based `unit-tests`, `integration-tests`, `E2E-test`,
  `docker-images`, `tarballs`, `publish-site`, and `build-docs`
  workflows.
- The `secp256k1` and `blst` install steps (only update them if
  proving FR1–FR4 turns out to require it; otherwise leave alone).
- Operator-facing commands or runtime behavior of the `moog`,
  `moog-oracle`, `moog-agent`, or `moog-mpfs-v2-canary` executables.

## Command / smoke distinction

This ticket ships no operator command. The "command" being repaired
is a CI workflow step; the "proof" is a `Build moog without nix`
workflow run URL on this branch that reaches `cabal build all`.

## Verification

- **Local gate (`./gate.sh`):** Haskell build/unit/fmt/hlint (inherited)
  plus `actionlint -shellcheck ''` on
  `.github/workflows/build-no-nix.yaml`. Catches workflow YAML and
  GitHub Actions schema regressions before push.
- **Live proof:** trigger `Build moog without nix` on this branch via
  `workflow_dispatch` (and naturally via the `pull_request` event for
  PRs targeting `main`) with the `~/.local` cache key invalidated, and
  attach the green run URL to the PR before marking ready.

## Open questions for plan phase (non-blocking for specs)

These belong in `plan.md`, not `spec.md`. Listed here only so they
are not lost:

- Is the failure root cause an unreachable `config.guess` /
  `config.sub` download from `autogen.sh`'s fallback path, or
  something else? Confirm with a reproducer before choosing a fix.
- Does pinning to a release tarball of the fork (containing
  pre-generated `config.sub`) avoid the autogen step entirely while
  satisfying FR3?
- Should the cache key embed the resolved `SODIUM_VERSION` so a fork
  revision bump in `iohk-nix` invalidates the cache automatically?
  (Out of scope unless it blocks FR1–FR4.)
