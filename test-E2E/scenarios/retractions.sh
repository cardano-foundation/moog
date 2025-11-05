#!/usr/bin/env bash

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/lib.sh"

export MOOG_WAIT=240
unset MOOG_TOKEN_ID

check

fund_wallets

log "Create an moog token"
being_oracle
result=$(moog oracle token boot)
tokenId=$(echo "$result" | jq -r '.value')
export MOOG_TOKEN_ID="$tokenId"
log "Moog token id $tokenId"

tokenEnd() {
    being_oracle
    log "Ending moog token $MOOG_TOKEN_ID..."
    moog oracle token end >/dev/null || echo "Failed to end the token"
}
trap 'tokenEnd' EXIT INT TERM

log "Register 'cfhal' as a GitHub user"
being_requester
result=$(moog requester register-user \
    --platform github \
    --username cfhal \
    --pubkeyhash  AAAAC3NzaC1lZDI1NTE5AAAAILjwzNvy87HbzYV2lsW3UjVoxtpq4Nrj84kjo3puarCH \
    )

log "Retract the user registration"
being_requester
outputRef=$(getOutputRef "$result")
moog retract -o "$outputRef"

result=$(moog token | jq '.requests')
if [[ "$result" == "[]" ]]; then
    log "Test passed: User registration successfully retracted"
else
    log "Test failed: Retraction not reflected in requests"
    exit 1
fi
