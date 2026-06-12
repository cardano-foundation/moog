#!/usr/bin/env bash
set -euo pipefail

nix develop --accept-flake-config -c bash -c '
  set -euo pipefail
  cabal update
  cabal build -O0 all
  cabal test unit-tests --test-show-details=direct
'
