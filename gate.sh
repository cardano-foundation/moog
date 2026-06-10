#!/usr/bin/env bash
set -euo pipefail

# Lifecycle sentinel for #175. Present = PR in flight; dropped in a
# `chore: drop gate.sh` commit before mark-ready. Run before every commit.

git diff --check
nix develop --quiet -c just build
nix develop --quiet -c just unit
nix develop --quiet -c cabal-fmt -c moog.cabal CI/rewrite-libs/rewrite-libs.cabal
nix develop --quiet -c fourmolu -m check src app test CI/rewrite-libs
nix develop --quiet -c just hlint
nix develop --quiet -c cabal check
