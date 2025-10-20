#!/usr/bin/env bash

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/lib.sh"

export ANTI_WAIT=240

unset ANTI_TOKEN_ID

log "Creating an anti token..."
result=$(anti oracle token boot)

tokenId=$(echo "$result" | jq -r '.value')
log "Anti token ID: $tokenId"

export ANTI_TOKEN_ID="$tokenId"

tokenEnd() {
    log "Ending anti token $ANTI_TOKEN_ID..."
    anti oracle token end >/dev/null || echo "Failed to end the token"
}
trap 'tokenEnd' EXIT INT TERM

##### Register a user
log "Registering a user..."
result=$(anti requester register-user \
    --platform github \
    --username paolino \
    --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8 \
    )
outputRef=$(getOutputRef "$result")

log "Pending requests for the anti oracle token:"
anti token | jq '.requests | .[]'


log "Updating the anti oracle token with output reference $outputRef..."
anti oracle token update -o "$outputRef" >/dev/null

printFacts

##### Register a role
log "Registering a role..."
result=$(anti requester register-role \
    --platform github \
    --repository cardano-foundation/antithesis \
    --username paolino \
    )
outputRef=$(getOutputRef "$result")

log "Pending requests for the anti oracle token:"
anti token | jq '.requests | .[]'

log "Updating the anti oracle token with output reference $outputRef..."
anti oracle token update -o "$outputRef" >/dev/null
printFacts

##### Request a test-run
log "Creating a test-run request..."
result=$(anti requester create-test \
    --platform github \
    --repository cardano-foundation/antithesis \
    --directory compose \
    --commit d9fb8d2bcfa321497ae3a89244bf13513a9a9a14 \
    --username paolino \
    --try 1 \
    --duration 3)
echo "$result"
outputRef=$(getOutputRef "$result")
log "Pending requests for the anti oracle token:"
anti token | jq '.requests | .[]'

log "Updating the anti oracle token with output reference $outputRef..."
anti oracle token update -o "$outputRef" >/dev/null

printFacts
