#!/usr/bin/env bash
set -euo pipefail

git diff --check
nix build --quiet .#unit-tests --no-link
