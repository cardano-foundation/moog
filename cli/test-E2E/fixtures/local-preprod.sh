#!/usr/bin/env bash

export MOOG_WALLET_FILE=tmp/test.json
export MOOG_TEST_REQUESTER_WALLET=tmp/test.json
export MOOG_TEST_ORACLE_WALLET=tmp/test.json
export MOOG_MPFS_HOST=http://localhost:3000
export MOOG_CONFIG_FILE=test-E2E/fixtures/moog-config.json
export MOOG_WAIT=180
export MOOG_SSH_FILE=test-E2E/fixtures/test_ed25519
export MOOG_SSH_PASSWORD=pw
address=$(moog wallet info | jq -r '.address')
echo "Funding address: $address"
curl -X 'POST' \
  'http://localhost:10000/local-cluster/api/addresses/topup' \
  -H 'accept: */*' \
  -H 'Content-Type: application/json' \
  -d '{
  "address": "'"$address"'",
  "adaAmount": 10000
}' | jq -r '.message'