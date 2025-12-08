#!/usr/bin/env bash

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/lib.sh"
unset MOOG_TOKEN_ID

export MOOG_WAIT=240

check

export_agent_public_key_hash

fund_wallets

log "Create an moog token"
being_oracle
result=$(moog oracle token boot)
tokenId=$(echo "$result" | jq -r '.value')
export MOOG_TOKEN_ID="$tokenId"
unset MOOG_SSH_FILE
log "Moog token id $tokenId"

tokenEnd() {
    being_oracle
    log "Ending moog token $MOOG_TOKEN_ID..."
    moog oracle token end >/dev/null || echo "Failed to end the token"
}
trap 'tokenEnd' EXIT INT TERM

log "Push the oracle config on-chain"
being_oracle
result=$(moog oracle config set \
    --min-test-duration 2 \
    --max-test-duration 4 \
    --agent-pkh "$MOOG_AGENT_PUBLIC_KEY_HASH")

echo "$result"

log "Include the config push"
include_requests

log "Change the oracle config to set min test duration to 1 hour"
being_oracle

result=$(moog oracle config set \
    --min-test-duration 1 \
    --max-test-duration 4 \
    --agent-pkh "$MOOG_AGENT_PUBLIC_KEY_HASH"
    )

echo "$result"

log "Include the config change"
include_requests

log "Register 'cfhal' as a GitHub user"
being_requester
result=$(moog requester register-user \
    --platform github \
    --username cfhal \
    --vkey vkey1lrqqrpr49593dv6jchcdlqpqj0y9rfpcaauscnhs74wc50z76aqsqqlrgh
    )

echo "$result"



log "Include the user registration"
include_requests

log "Register cfhal as cardano-foundation/hal-fixture-sin repository antithesis test run requester"
being_requester
result=$(moog requester register-role \
    --platform github \
    --username cfhal \
    --repository cardano-foundation/hal-fixture-sin
)
echo "$result"

log "Include the role registration"
include_requests

log "Whitelist the cardano-foundation/hal-fixture-sin repository"
being_agent
result=$(moog agent white-list \
    --platform github \
    --repository cardano-foundation/hal-fixture-sin
)
echo "$result"

log "Include the repository whitelisting"
include_requests

log "Register a test run from cfhal to run an antithesis test on the cardano-foundation/hal-fixture-sin repository, first try"
being_requester
result=$(moog requester create-test \
    --platform github \
    --username cfhal \
    --repository cardano-foundation/hal-fixture-sin \
    --directory antithesis-test \
    --commit a7741a44dfddfe05822e1a49862ceea43ecd657d \
    --try 1 \
    --duration 1)

testRunId=$(echo "$result" | jq -r '.value.testRunId')

# Validate that testRunId is exactly 64 characters long
if [[ -z "$testRunId" || ${#testRunId} -ne 64 ]]; then
    echo "Invalid testRunId length: ${#testRunId}" >&2
    echo "$result"
    exit 1
fi

log "Include the test run registration"
include_requests

log "Reject the test run with no reasons..."
being_agent
validation=$(moog agent query)
references=$(echo "$validation" | jq -r '.pending | .[] | .id')
moog agent reject-test -i "$references" > /dev/null

log "Include the test run rejection"
include_requests

log "Create a new test run request for the same repository, directory, and commit and duration, second try"
being_requester
moog requester create-test \
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
validation=$(moog agent query)
reference=$(echo "$validation" | jq -r '.pending | .[] | .id')
moog agent accept-test -i "$reference" > /dev/null

log "Include the test run acceptance"
include_requests

log "Finish the test run"
being_agent
validation=$(moog agent query)
moog agent report-test -i "$reference" \
    --duration 1 \
    --url "https://example.com/report" \
    --outcome "success"

log "Include the test run report"
include_requests

validation=$(moog agent query)

being_requester
log "Facts:"
url=$(moog facts test-runs 'done' --test-run-id "$reference" | jq -r '.[] | .value.url')
if [[ "$url" != "https://example.com/report" ]]; then
    echo "Unexpected test run report URL: $url"
    exit 1
fi
