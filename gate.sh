#!/usr/bin/env bash
set -euo pipefail

git diff --check
nix develop --quiet -c just build
nix develop --quiet -c just unit
nix develop --quiet -c cabal-fmt -c moog.cabal CI/rewrite-libs/rewrite-libs.cabal
nix develop --quiet -c fourmolu -m check src app test CI/rewrite-libs
nix develop --quiet -c just hlint

# Workflow YAML touched by this PR must remain syntactically valid.
# Shellcheck warnings on this file pre-date the libsodium fix and are not
# in scope; only validate YAML + GitHub Actions schema.
nix shell nixpkgs#actionlint --quiet -c \
  actionlint -shellcheck '' .github/workflows/build-no-nix.yaml
