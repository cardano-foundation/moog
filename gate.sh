#!/usr/bin/env bash
# gate.sh — mechanical gate for the MPFS v2 readiness-gate canary slice
# (paired with cardano-mpfs-offchain#276 / issue #275).
# Ephemeral; dropped in the final commit before the PR is marked ready.
set -euo pipefail

git diff --check

nix develop --quiet -c just build
nix develop --quiet -c just unit
nix develop --quiet -c cabal-fmt -c moog.cabal CI/rewrite-libs/rewrite-libs.cabal
nix develop --quiet -c fourmolu -m check src app test CI/rewrite-libs
nix develop --quiet -c just hlint

# Validate the workflow yaml we touch in this PR.
nix shell nixpkgs#actionlint --quiet -c \
  actionlint -shellcheck '' \
    .github/workflows/mpfs-v2-canary.yaml

echo "gate: OK"
