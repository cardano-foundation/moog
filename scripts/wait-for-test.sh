#!/usr/bin/env bash

# Usage:
# wait-for-test.sh 175672783fab56ace66e5463d8b159a9411264847095b4d50b8dae2bb620cb3b
# exits with 0 when finished or 1 if rejected
set -euo pipefail

ID="$1"

function query_run() { moog facts test-runs --test-run-id "$ID" | jq '.[0]'; }

echo "waiting to be accepted..."
while true; do
  STATUS=$(query_run | jq -r .value.phase)
  case $STATUS in
    accepted)
      echo "accepted"
      break;
      ;;
    rejected)
      echo "rejected"
      exit 1
      ;;
    finished)
      echo "already finished"
      break;
      ;;
    pending)
      ;;
    *)
      echo "unknown status: $STATUS"
      ;;
  esac
  sleep 10
  echo "..."
done

echo "waiting to be finished..."
while true; do
  STATUS=$(query_run | jq -r .value.phase)
  case $STATUS in
    finished)
      echo "finished"
      break;
      ;;
    accepted)
      ;;
    *)
      echo "unknown status: $STATUS"
      ;;
  esac
  sleep 60
  echo "..."
done

case $(query_run | jq -r .value.outcome) in
  success)
    exit 0;
    ;;
  failure)
    echo "failed"
    exit 1
    ;;
  *) # includes "unknown"
    echo "unknown outcome"
    exit 1
esac
