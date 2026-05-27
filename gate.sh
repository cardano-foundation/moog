#!/usr/bin/env bash
set -euo pipefail

# Build + run unit tests via the same flake output CI uses (avoids the
# currently-broken `nix develop` shell on main — cabal-fmt 0.1.12 doesn't
# allow base 4.21 from the GHC 9.12 bump). Lint/format are not part of CI
# today; reintroduce here once the dev shell is repaired.
git diff --check
nix --quiet run .#unit-tests
