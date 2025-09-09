#!/usr/bin/env bash

set -o pipefail

SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin"

# Environment variables
POOLS="${POOLS:-}"
mapfile -t NODES < <(seq -f "p%g" 1 "$POOLS")
set -f
NODES+=( $EXTRA_NODES )
set +f

PORT="${PORT:-3001}"
NETWORKMAGIC="${NETWORKMAGIC:-42}"
LIMIT="${LIMIT:-100}"

echo "Checking flaky chain sync among the following nodes"
printf '%s\n' "${NODES[@]}"

# TODO: select randomly
TESTED_POOL="${NODES[0]}"

# We may want to move this logic to the haskell code
if [[ -f "$CHAINPOINT_FILEPATH" ]]; then
  POINT=$(cat $CHAINPOINT_FILEPATH | sort | uniq | shuf | head -1)
else
  POINT="origin"
fi

adversary "$NETWORKMAGIC" "$TESTED_POOL" "$PORT" "$LIMIT" "$POINT" 100
