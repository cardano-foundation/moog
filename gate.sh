#!/usr/bin/env bash
set -euo pipefail

git diff --check
nix build --quiet --no-link .#moog-agent .#unit-tests
