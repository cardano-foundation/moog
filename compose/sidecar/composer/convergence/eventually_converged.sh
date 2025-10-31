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


echo "Checking for convergence among the following nodes"
printf '%s\n' "${NODES[@]}"

verify_environment_variables() {
    if [ -z "${POOLS}" ]; then
        echo "POOLS not defined, exiting..."
        sleep 60
        exit 1
    fi
}

validate_block_hash() {
    temp_dir=$(mktemp -d)
    pids=()

    # Fetch hashes in parallel
    for i in "${NODES[@]}"; do
        (
            timeout 10 cardano-cli ping -j --magic 42 --host "${i}.example" --port "${PORT}" --tip --quiet -c1 | jq -r '.tip[0].hash + " " + (.tip[0].blockNo|tostring) + " " + (.tip[0].slotNo|tostring)' >"$temp_dir/hash_${i}"
        )2>/dev/null &
        # store background process pid and corresponding node being checked
        pids+=("$i $!")
    done

    # Wait for all processes and handle errors
    for pid in "${pids[@]}"; do
        # extract pid to wait on and node id
        read -ra check <<< "$pid"
        if ! wait "${check[1]}"; then
            echo "Error: Checking node ${check[0]} failed" >&2
            status=2
        fi
    done

    # Analyze results
    hash_files=("$temp_dir"/hash_*)
    hash_count=$(cut -d' ' -f1 "${hash_files[@]}" | sort | uniq | wc -l)

    if [ "${status}" -eq 2 ]; then
        status=1
    elif [ "$hash_count" -eq 1 ]; then
        # All hashes match
        common_line=$(head -n1 "${hash_files[0]}")
        read -r hash block slot <<< "$common_line"
        message="[{\"status\":\"synced\",\"hash\":\"${hash}\",\"block\":\"${block}\",\"slot\":\"${slot}\"}]"
        status=0
    else
        # Hash mismatch detected
        for file in "${hash_files[@]}"; do
            pool_id=$(basename "$file" | sed 's/hash_//')
            read -r hash block slot <<< "$(cat "$file")"
            message="[{\"status\":\"diverged\",\"hash\":\"${hash}\",\"block\":\"${block}\",\"slot\":\"${slot}\",\"pool_id\":\"${pool_id}\"}]"
            echo "${message}"
        done
        status=1
    fi
}

# Establish run order
main() {
    verify_environment_variables
    while true; do
        status=1

        # Try up to ~16 minutes for status to converge, in slices of 10 seconds
        # The reason we wait for 15 minutes is because that's the default timeout for
        # failed DNS resolutions and connections errors baked in the node network
        # layer.
        for i in {1..100}; do
            validate_block_hash
            if [ "${status}" -eq 0 ]; then
                break
            else
                sleep 10
            fi
        done

        if [ "${status}" -eq 0 ]; then
            echo "${message}"
            exit 0
        else
            echo "[{\"status\":\"diverged\"}]"
            exit 1
        fi
    done
}

main
