#!/usr/bin/env bash
set -euo pipefail

git diff --check
nix run .#unit-tests
