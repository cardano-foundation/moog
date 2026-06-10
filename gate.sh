#!/usr/bin/env bash
set -euo pipefail

# Lifecycle sentinel for #175. Present = PR in flight; dropped in a
# `chore: drop gate.sh` commit before mark-ready. Run before every commit.
#
# NOTE: the project dev shell (`nix develop`) is UNSOLVABLE at this commit —
# cabal-fmt resolves to a base<4.20 build while the flake pins ghc9123
# (base 4.21) with no allow-newer for it. That is a pre-existing infra break
# from the ghc9123 bump, NOT owned by #175. So this gate uses the same path CI
# uses for correctness (`nix run .#unit-tests`, the only PR check that gates
# code) and nixpkgs fourmolu/hlint for style — the project's pinned
# fourmolu/hlint live only inside the broken shell, and moog CI does not gate
# on them. Style is scoped to the files this PR changes so pre-existing,
# tool-version drift elsewhere cannot false-fail the gate.

git diff --check

# Correctness — identical to the unit-tests.yaml pull_request check.
nix --quiet run .#unit-tests

# Style tools: prefer the project dev shell if it ever becomes solvable,
# otherwise fall back to nixpkgs (fourmolu 0.19 ≈ the ghc9123-era pin). Both
# honour the repo fourmolu.yaml / .hlint.yaml regardless of tool version.
if nix develop --quiet -c true 2>/dev/null; then
    fmt() { nix develop --quiet -c fourmolu "$@"; }
    lint() { nix develop --quiet -c hlint "$@"; }
else
    echo "gate: dev shell unsolvable (cabal-fmt/ghc9123) — using nixpkgs fourmolu/hlint"
    fmt() { nix run --quiet nixpkgs#fourmolu -- "$@"; }
    lint() { nix run --quiet nixpkgs#hlint -- "$@"; }
fi

changed_hs=$(
    {
        git diff --name-only --diff-filter=ACMR origin/main...HEAD -- '*.hs'
        git diff --name-only --diff-filter=ACMR -- '*.hs'
    } | sort -u
)
if [ -n "$changed_hs" ]; then
    # shellcheck disable=SC2086
    fmt -m check $changed_hs
    # shellcheck disable=SC2086
    lint $changed_hs
else
    echo "gate: no changed .hs files to style-check"
fi
