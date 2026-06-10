#!/usr/bin/env bash
set -euo pipefail

# Lifecycle sentinel for #175. Present = PR in flight; dropped in a
# `chore: drop gate.sh` commit before mark-ready. Run before every commit.
#
# Correctness is gated exactly as CI gates it: `nix run .#unit-tests` is the
# only code check `unit-tests.yaml` (pull_request) runs. moog CI runs NO
# fourmolu/hlint/cabal-fmt job.
#
# Style is NOT auto-gated here, on purpose:
#   * The project's pinned fourmolu/hlint live only inside `nix develop`, which
#     is UNSOLVABLE at this commit (cabal-fmt wants base<4.20, the flake pins
#     ghc9123/base 4.21, no allow-newer) — a pre-existing infra break from the
#     ghc9123 bump, out of scope for #175.
#   * nixpkgs fourmolu 0.19 is a *different* version: run extension-aware it
#     reformats 16 hunks of pre-existing, untouched HEAD code (agentProcess,
#     import blocks). Using it would churn code this PR never changed.
# So new code is hand-formatted to the project style (70-col, leading
# commas/arrows, haddock on exports) and style-reviewed in the diff. A project
# `just format` / `just hlint` pass is a follow-up once the dev shell is fixed
# (tracked separately). See specs/175-…/plan.md and WIP.md.

git diff --check

# Correctness — identical to the unit-tests.yaml pull_request check.
nix --quiet run .#unit-tests
