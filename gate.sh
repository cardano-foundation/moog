#!/usr/bin/env bash
set -euo pipefail

git diff --check
cabal build all --enable-tests
cabal test unit-tests --test-show-details=direct
cabal-fmt -c moog.cabal CI/rewrite-libs/rewrite-libs.cabal
fourmolu -m check src app test CI/rewrite-libs
hlint -c src app test CI/rewrite-libs

