#!/usr/bin/env bash

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/lib.sh"
unset ANTI_TOKEN_ID

export ANTI_WAIT=240

check

export_agent_public_key_hash

fund_wallets

log "Create an anti token"
being_oracle
result=$(anti oracle token boot)
tokenId=$(echo "$result" | jq -r '.value')
export ANTI_TOKEN_ID="$tokenId"
log "Anti token id $tokenId"

tokenEnd() {
    being_oracle
    log "Ending anti token $ANTI_TOKEN_ID..."
    anti oracle token end >/dev/null || echo "Failed to end the token"
}
trap 'tokenEnd' EXIT INT TERM

log "Push the oracle config on-chain"
being_oracle
anti oracle config set \
    --min-test-duration 1 \
    --max-test-duration 4 \
    --agent-pkh "$ANTI_AGENT_PUBLIC_KEY_HASH"

log "Register 'cfhal' as a GitHub user"
being_requester
anti requester register-user \
    --platform github \
    --username cfhal \
    --pubkeyhash  AAAAC3NzaC1lZDI1NTE5AAAAILjwzNvy87HbzYV2lsW3UjVoxtpq4Nrj84kjo3puarCH \
    > /dev/null

log "Include the user registration"
include_requests

log "Register cfhal as cardano-foundation/hal-fixture-sin repository antithesis test run requester"
being_requester
anti requester register-role \
    --platform github \
    --username cfhal \
    --repository cardano-foundation/hal-fixture-sin \
    > /dev/null

log "Include the role registration"
include_requests

log "Whitelist the cardano-foundation/hal-fixture-sin repository"
being_agent
anti agent white-list \
    --platform github \
    --repository cardano-foundation/hal-fixture-sin

log "Include the repository whitelisting"
include_requests

log "Register a test run from cfhal to run an antithesis test on the cardano-foundation/hal-fixture-sin repository, first try"
being_requester
anti requester create-test \
    --platform github \
    --username cfhal \
    --repository cardano-foundation/hal-fixture-sin \
    --directory antithesis-test \
    --commit a7741a44dfddfe05822e1a49862ceea43ecd657d \
    --try 1 \
    --duration 1

log "Include the test run registration"
include_requests

log "Reject the test run with no reasons..."
being_agent
validation=$(anti agent query)
references=$(echo "$validation" | jq -r '.pending | .[] | .id')
anti agent reject-test -i "$references" > /dev/null

log "Include the test run rejection"
include_requests

log "Create a new test run request for the same repository, directory, and commit and duration, second try"
being_requester
anti requester create-test \
    --platform github \
    --username cfhal \
    --repository cardano-foundation/hal-fixture-sin \
    --directory antithesis-test \
    --commit a7741a44dfddfe05822e1a49862ceea43ecd657d \
    --try 2 \
    --duration 1

log "Include the new test run request"
include_requests

log "Accept the new test run request because it's a scenario"
being_agent
validation=$(anti agent query)
reference=$(echo "$validation" | jq -r '.pending | .[] | .id')
anti agent accept-test -i "$reference" > /dev/null

log "Include the test run acceptance"
include_requests

log "Finish the test run"
being_agent
validation=$(anti agent query)
anti agent report-test -i "$reference" \
    --duration 1 \
    --url "https://example.com/report"

log "Include the test run report"
include_requests

validation=$(anti agent query)

being_requester
log "Facts:"
url=$(anti facts test-runs 'done' --test-run-id "$reference" | jq -r '.[] | .value.url')
if [[ "$url" != "https://example.com/report" ]]; then
    echo "Unexpected test run report URL: $url"
    exit 1
fi
