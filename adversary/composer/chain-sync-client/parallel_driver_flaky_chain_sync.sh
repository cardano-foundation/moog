#!/usr/bin/env bash

set -o pipefail

SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$PATH"

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

adversary "$NETWORKMAGIC" "$PORT" "$LIMIT" "$CHAINPOINT_FILEPATH" 100 "${NODES[@]}"
