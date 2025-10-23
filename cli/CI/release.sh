#!/usr/bin/env bash

set -euo pipefail

if [ $# -ne 2 ]; then
    echo "Usage: $0 <release> <platform>"
    echo "Example: $0 v1.0.0 linux64"
    echo "Supported platforms: linux64, darwin64"
    exit 1
fi


if ! command -v gh >/dev/null 2>&1; then
    echo "Error: GitHub CLI (gh) is not installed or not in PATH."
    exit 1
fi

if ! gh auth status >/dev/null 2>&1; then
    echo "Error: GitHub CLI (gh) is not authenticated. Please run 'gh auth login'."
    exit 1
fi

release=$1
platform=$2

if ! gh release view "$release" >/dev/null 2>&1; then
    echo "Error: Release '$release' does not exist."
    echo "Do you want to create it? (y/n)"
    read -r create_release
    if [[ "$create_release" == "y" ]]; then
        gh release create "$release" --title "$release" --notes "Release $release" --draft
        echo "Release '$release' created."
    else
        echo "Exiting without creating the release."
        exit 1
    fi
fi

version=$(nix eval .#version --raw)
echo "Building moog artifacts version $version"
nix build ".#$platform.tarball"

tarball=result/moog-$version-$platform.tar.gz
echo "Tarball path: $tarball"

mktempdir=$(mktemp -d)
releaseTarball="$mktempdir/moog-$release-$platform.tar.gz"

cp -L "$tarball" "$releaseTarball"

gh release upload "$release" "$releaseTarball"

echo "Release URL: https://github.com/cardano-foundation/antithesis/releases/tag/$release"