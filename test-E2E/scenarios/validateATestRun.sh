#!/usr/bin/env bash

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/lib.sh"

export MOOG_WAIT=240

unset MOOG_TOKEN_ID

log "Creating an moog token..."
result=$(moog oracle token boot)

tokenId=$(echo "$result" | jq -r '.value')
log "Moog token ID: $tokenId"

export MOOG_TOKEN_ID="$tokenId"

tokenEnd() {
    log "Ending moog token $MOOG_TOKEN_ID..."
    moog oracle token end >/dev/null || echo "Failed to end the token"
}
trap 'tokenEnd' EXIT INT TERM

##### Register a user
log "Registering a user..."
result=$(moog requester register-user \
    --platform github \
    --username paolino \
    --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8 \
    )
outputRef=$(getOutputRef "$result")

log "Pending requests for the moog oracle token:"
moog token | jq '.requests | .[]'


log "Updating the moog oracle token with output reference $outputRef..."
moog oracle token update -o "$outputRef" >/dev/null

printFacts

##### Register a role
log "Registering a role..."
result=$(moog requester register-role \
    --platform github \
    --repository cardano-foundation/moog \
    --username paolino \
    )
outputRef=$(getOutputRef "$result")

log "Pending requests for the moog oracle token:"
moog token | jq '.requests | .[]'

log "Updating the moog oracle token with output reference $outputRef..."
moog oracle token update -o "$outputRef" >/dev/null
printFacts

##### Request a test-run
log "Creating a test-run request..."
result=$(moog requester create-test \
    --platform github \
    --repository cardano-foundation/moog \
    --directory compose \
    --commit d9fb8d2bcfa321497ae3a89244bf13513a9a9a14 \
    --username paolino \
    --try 1 \
    --duration 3)
echo "$result"
outputRef=$(getOutputRef "$result")
log "Pending requests for the moog oracle token:"
moog token | jq '.requests | .[]'

log "Updating the moog oracle token with output reference $outputRef..."
moog oracle token update -o "$outputRef" >/dev/null

printFacts
