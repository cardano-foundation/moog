#!/usr/bin/env bash
#
# Source this from the moog repo root to set up a LOCAL hermetic e2e run
# against the self-booted offchain mpfs-devnet-server. The harness (test-E2E
# slice A) now boots the devnet-server and funds genesis itself, so there is
# no docker-compose / yaci / topup anymore:
#
#   source test-E2E/fixtures/local-preprod.sh
#   nix run .#e2e-tests
#
# Override OFFCHAIN_DIR if your cardano-mpfs-offchain checkout lives
# elsewhere (default /code/cardano-mpfs-offchain).

# Wallet the scenarios sign with. Point all three role vars at one wallet;
# the harness de-dups their addresses and funds them at genesis. Use the
# GitHub-registered cfhal wallet if you want the register-user / register-role
# GitHub checks to pass; a freshly created wallet is fine for the MPFS-only
# assertions.
export MOOG_WALLET_FILE=${MOOG_WALLET_FILE:-tmp/test.json}
export MOOG_TEST_REQUESTER_WALLET=$MOOG_WALLET_FILE
export MOOG_TEST_ORACLE_WALLET=$MOOG_WALLET_FILE
export MOOG_TEST_AGENT_WALLET=$MOOG_WALLET_FILE

# Optional config / ssh inputs some flows read.
export MOOG_CONFIG_FILE=test-E2E/fixtures/moog-config.json
export MOOG_SSH_FILE=test-E2E/fixtures/test_ed25519
export MOOG_SSH_PASSWORD=pw

# A GitHub PAT with public-repo read access is required: the register-user
# flow validates the cfhal vkey against GitHub. Export MOOG_GITHUB_PAT first.
if [ -z "${MOOG_GITHUB_PAT:-}" ]; then
    echo "WARNING: MOOG_GITHUB_PAT is unset; register-user GitHub checks will fail." >&2
fi

# Build the paired devnet-server from the offchain rev pinned in
# cabal.project (client + server stay the same version) and capture the
# blueprint / genesis / PATH from its devshell, exactly as the E2E CI does.
OFFCHAIN_DIR=${OFFCHAIN_DIR:-/code/cardano-mpfs-offchain}
rev=$(awk '/cardano-mpfs-offchain\.git/{f=1} f&&/^[[:space:]]*tag:/{print $2; exit}' cabal.project)
offchain="git+file://${OFFCHAIN_DIR}?rev=${rev}"

server_store=$(nix --quiet build --print-out-paths "${offchain}#mpfs-devnet-server")
export MPFS_DEVNET_SERVER="${server_store}/bin/mpfs-devnet-server"

# shellcheck disable=SC2016
mapfile -t _devnet_env < <(
    nix --quiet develop "${offchain}" -c bash -c \
        'printf "%s\n%s\n%s\n" "$MPFS_BLUEPRINT" "$E2E_GENESIS_DIR" "$PATH"'
)
export MPFS_BLUEPRINT="${_devnet_env[0]}"
export E2E_GENESIS_DIR="${_devnet_env[1]}"
export MPFS_DEVNET_PATH="${_devnet_env[2]}"

echo "Devnet env ready (offchain rev ${rev}). Now run: nix run .#e2e-tests"
