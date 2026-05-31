#!/usr/bin/env bash

set -euo pipefail

cat >&2 <<'EOF'
CI/release.sh has been retired.

Create or push a v* tag to run the GitHub release workflows:
  - .github/workflows/release.yml
  - .github/workflows/darwin-release.yml

The old combined moog-<version>-<platform>.tar.gz assets are no longer built.
Release assets are now per executable and per platform.
EOF

exit 1
