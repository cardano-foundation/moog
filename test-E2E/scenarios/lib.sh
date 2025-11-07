#!/usr/bin/env bash

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1"
}

check() {
    if ! command -v moog &> /dev/null; then
        echo "moog command not found, please make it available in your PATH"
        exit 1
    fi
    if [ -z "$MOOG_WALLET_FILE" ]; then
        log "Error: MOOG_WALLET_FILE is not set."
        exit 1
    fi

    if [ ! -f "$MOOG_WALLET_FILE" ]; then
        log "Error: MOOG_WALLET_FILE does not exist at $MOOG_WALLET_FILE."
        exit 1
    fi

    if ! command -v moog &>/dev/null; then
        log "Error: 'moog' command is not available. Please ensure it is installed and in your PATH."
        exit 1
    fi
    if [ -z "${MOOG_MPFS_HOST:-}" ]; then
        echo "Please set MOOG_MPFS_HOST environment variable, this is the host of the moog MPFS server"
        exit 1
    fi
    if [ -z "${MOOG_TEST_ORACLE_WALLET:-}" ]; then
        echo "Please set MOOG_TEST_ORACLE_WALLET environment variable"
        exit 1
    fi
    if [ -z "${MOOG_TEST_AGENT_WALLET:-}" ]; then
        echo "Please set MOOG_TEST_AGENT_WALLET environment variable"
        exit 1
    fi
    if [ -z "${MOOG_TEST_REQUESTER_WALLET:-}" ]; then
        echo "Please set MOOG_TEST_REQUESTER_WALLET environment variable"
        exit 1
    fi
    if [ -z "${MOOG_GITHUB_PAT:-}" ]; then
        echo "Please set MOOG_GITHUB_PAT environment variable, this is a valid GitHub personal access token with access to the public github API \
            Or use the pat available in 1password vault at the same record."
        exit 1
    fi
}

printFacts() {
    log "Current facts:"
    moog facts | jq '.[]'
}

getOutputRef() {
    # shellcheck disable=SC2155
    local txHash=$(echo "$1" | jq -r '.txHash')
    echo "${txHash}-0"
}

fund () {
    export MOOG_WALLET_FILE=$1
    address=$(moog wallet info | jq -r '.address')
    log "Funding address: $address"
    curl -s -X 'POST' \
        "$MOOG_TEST_YACI_ADMIN/local-cluster/api/addresses/topup" \
        -H 'accept: */*' \
        -H 'Content-Type: application/json' \
        -d '{
        "address": "'"$address"'",
        "adaAmount": 10000
        }' > /dev/null
}

fund_wallets(){
    if [ -z "${MOOG_TEST_YACI_ADMIN:-}" ]; then
        echo "MOOG_TEST_YACI_ADMIN environment variable is not set, skipping funding wallets"
    else
        fund "$MOOG_TEST_REQUESTER_WALLET"
        fund "$MOOG_TEST_ORACLE_WALLET"
        fund "$MOOG_TEST_AGENT_WALLET"
    fi
}

being_oracle (){
    export MOOG_WALLET_FILE=$MOOG_TEST_ORACLE_WALLET
    }
being_agent (){
    export MOOG_WALLET_FILE=$MOOG_TEST_AGENT_WALLET
    }
being_requester (){
    export MOOG_WALLET_FILE=$MOOG_TEST_REQUESTER_WALLET
    }

include_requests() {
    being_oracle
    validation=$(moog token)
    references=$(echo "$validation" | jq -r '.requests | .[] | select(.validation == "validated") | .request.outputRefId')
    if [ -z "$references" ]; then
        log "No references validated: $validation"
        exit 1
    fi
    # shellcheck disable=SC2046
    moog oracle token update $(echo "$references" | xargs -I {} echo -o {}) > /dev/null
}

export_agent_public_key_hash() {
    being_agent
    agent=$(moog wallet info | jq -r '.owner')
    export MOOG_AGENT_PUBLIC_KEY_HASH=$agent
}
