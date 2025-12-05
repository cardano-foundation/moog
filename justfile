# shellcheck shell=bash

# shellcheck disable=SC2121
set unstable := true

format:
    #!/usr/bin/env bash
    # shellcheck disable=SC2034
    for i in {1..3}; do
        fourmolu -i src app test test-E2E test-integration test-lib CI/rewrite-libs
    done
    cabal-fmt -i moog.cabal CI/rewrite-libs/rewrite-libs.cabal
    nixfmt *.nix
    nixfmt nix/*.nix
    nixfmt CI/rewrite-libs/*.nix
    nixfmt CI/rewrite-libs/nix/*.nix

hlint:
    #!/usr/bin/env bash
    hlint app src test test-E2E test-lib CI/rewrite-libs

unit match="":
    #!/usr/bin/env bash
    # shellcheck disable=SC2050
    if [[ '{{ match }}' == "" ]]; then
      cabal test unit-tests \
          --test-show-details=direct
    else
      cabal test unit-tests\
          --test-show-details=direct \
          --test-option=--match \
          --test-option="{{ match }}"
    fi

build:
    #!/usr/bin/env bash
    cabal build all --enable-tests

integration match="":
    #!/usr/bin/env bash
    # shellcheck disable=SC2050
    mkdir -p tmp/bin
    cabal install --overwrite-policy=always --installdir=tmp >/dev/null
    export PATH="$PWD/tmp:$PATH"
    if [ -z "${MOOG_GITHUB_PAT:-}" ]; then
        echo "Please set MOOG_GITHUB_PAT environment variable, this is a valid GitHub personal access token with access to the public github API"
        exit 1
    fi
    if [ -z "${MOOG_SSH_PASSWORD:-}" ]; then
        echo "Please set MOOG_SSH_PASSWORD environment variable, this is the passphrase for the cfhal encrypted SSH private key"
        exit 1
    fi
    if ! test -f  tmp/test.json; then
        echo "E2E tests expect wallet definition in tmp/test.json file"
        exit 1
    fi
    randomMPFSPort=$(shuf -i 1024-65636 -n 1)
    export MOOG_MPFS_HOST="http://localhost:$randomMPFSPort"
    export MPFS_PORT="$randomMPFSPort"
    randomYaciAdminPort=$(shuf -i 1024-65536 -n 1)
    export YACI_ADMIN_PORT="$randomYaciAdminPort"
    export MOOG_TEST_YACI_ADMIN="http://localhost:$YACI_ADMIN_PORT"

    # shellcheck disable=SC2002
    randomName=$(cat /dev/urandom | tr -dc 'a-z0-9' | fold -w 8 | head -n 1)
    docker compose -p "$randomName" -f test-E2E/fixtures/docker-compose.yml up -d
    down() {
      docker compose -p "$randomName" \
        -f test-E2E/fixtures/docker-compose.yml down
    }
    trap down EXIT INT

    export MOOG_WALLET_FILE=tmp/requester.json
    export MOOG_TEST_REQUESTER_WALLET=tmp/requester.json # this must be cfhal wallet, get it in 1p at https://start.1password.com/open/i?a=TYQQQLKUDBAFVHQ4P7XKFCUVYM&v=fhipthmhnufti4q2kky6d7336u&i=jgle6xhz6b4pgma7aodrsebfgu&h=cardanofoundation.1password.com
    export MOOG_TEST_ORACLE_WALLET=tmp/requester.json # or create your own wallet
    export MOOG_TEST_AGENT_WALLET=tmp/requester.json # or create your own wallet
    export MOOG_SSH_FILE=test-E2E/fixtures/cfhal_ed25519
    export MOOG_WAIT=2
    while [[ "$(curl -s "localhost:$MPFS_PORT/tokens" | jq -r '.indexerStatus.ready')" != "true" ]]; do
        echo "Waiting for indexer to be ready..."
        sleep 2
    done
    address=$(moog wallet info | jq -r '.address')
    echo "Funding address: $address"
    topup(){
        curl -s -X 'POST' \
            "$MOOG_TEST_YACI_ADMIN/local-cluster/api/addresses/topup" \
            -H 'accept: */*' \
            -H 'Content-Type: application/json' \
            -d '{
            "address": "'"$address"'",
            "adaAmount": 10000
            }'
        }
    while true; do
        if topup | grep -q "Topup successful"; then
            break
        fi
        echo "Retrying topup..."
        sleep 2
    done
    owner=$(moog wallet info | jq -r '.owner')
    export MOOG_AGENT_PUBLIC_KEY_HASH=$owner
    if [[ '{{ match }}' == "" ]]; then
      cabal test integration-tests \
          --test-show-details=direct
    else
      cabal test integration-tests \
          --test-show-details=direct \
          --test-option=--match \
          --test-option="{{ match }}"
    fi
E2E match="":
    #!/usr/bin/env bash
    mkdir -p tmp/bin
    cabal install --overwrite-policy=always --installdir=tmp >/dev/null
    export PATH="$PWD/tmp:$PATH"
    if [ -z "${MOOG_GITHUB_PAT:-}" ]; then
        echo "Please set MOOG_GITHUB_PAT environment variable, this is a valid GitHub personal access token with access to the public github API"
        exit 1
    fi
    if [ -z "${MOOG_SSH_PASSWORD:-}" ]; then
        echo "Please set MOOG_SSH_PASSWORD environment variable, this is the passphrase for the cfhal encrypted SSH private key"
        exit 1
    fi
    if ! test -f  tmp/test.json; then
        echo "E2E tests expect wallet definition in tmp/test.json file"
        exit 1
    fi
    randomMPFSPort=$(shuf -i 1024-65636 -n 1)
    export MOOG_MPFS_HOST="http://localhost:$randomMPFSPort"
    export MPFS_PORT="$randomMPFSPort"
    randomYaciAdminPort=$(shuf -i 1024-65536 -n 1)
    export YACI_ADMIN_PORT="$randomYaciAdminPort"
    export MOOG_TEST_YACI_ADMIN="http://localhost:$YACI_ADMIN_PORT"

    # shellcheck disable=SC2002
    randomName=$(cat /dev/urandom | tr -dc 'a-z0-9' | fold -w 8 | head -n 1)
    docker compose -p "$randomName" -f test-E2E/fixtures/docker-compose.yml up -d
    down() {
      docker compose -p "$randomName" \
        -f test-E2E/fixtures/docker-compose.yml down
    }
    trap down EXIT INT

    export MOOG_WALLET_FILE=tmp/requester.json
    export MOOG_TEST_REQUESTER_WALLET=tmp/requester.json # this must be cfhal wallet, get it in 1p at https://start.1password.com/open/i?a=TYQQQLKUDBAFVHQ4P7XKFCUVYM&v=fhipthmhnufti4q2kky6d7336u&i=jgle6xhz6b4pgma7aodrsebfgu&h=cardanofoundation.1password.com
    export MOOG_TEST_ORACLE_WALLET=tmp/requester.json # or create your own wallet
    export MOOG_TEST_AGENT_WALLET=tmp/requester.json # or create your own wallet
    export MOOG_SSH_FILE=test-E2E/fixtures/cfhal_ed25519
    export MOOG_WAIT=2
    while [[ "$(curl -s "localhost:$MPFS_PORT/tokens" | jq -r '.indexerStatus.ready')" != "true" ]]; do
        echo "Waiting for indexer to be ready..."
        sleep 2
    done
    address=$(moog wallet info | jq -r '.address')
    echo "Funding address: $address"
    topup(){
        curl -s -X 'POST' \
            "$MOOG_TEST_YACI_ADMIN/local-cluster/api/addresses/topup" \
            -H 'accept: */*' \
            -H 'Content-Type: application/json' \
            -d '{
            "address": "'"$address"'",
            "adaAmount": 10000
            }'
        }
    while true; do
        if topup | grep -q "Topup successful"; then
            break
        fi
        echo "Retrying topup..."
        sleep 2
    done
    owner=$(moog wallet info | jq -r '.owner')
    export MOOG_AGENT_PUBLIC_KEY_HASH=$owner
    echo "Starting E2E tests..."
    just E2E-tests "{{ match }}"

E2E-tests match="":
    #!/usr/bin/env bash
    # shellcheck disable=SC2050
    if [[ '{{ match }}' == "" ]]; then
    cabal test e2e-tests \
        --test-show-details=direct
    else
    cabal test e2e-tests \
        --test-show-details=direct \
        --test-option=--match \
        --test-option="{{ match }}"
    fi

CI:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just unit
    just integration
    just E2E
    cabal-fmt -c moog.cabal CI/rewrite-libs/rewrite-libs.cabal
    fourmolu -m check src app test CI/rewrite-libs
    hlint -c src app test CI/rewrite-libs

start-mpfs:
    #!/usr/bin/env bash
    docker compose -p "moog-testing" -f test-E2E/fixtures/docker-compose.yml up -d

stop-mpfs:
    #!/usr/bin/env bash
    docker compose -p "moog-testing" -f test-E2E/fixtures/docker-compose.yml down

build-moog-agent tag='latest':
    #!/usr/bin/env bash
    nix build .#moog-agent-docker-image
    docker load < result
    version=$(nix eval --raw .#version)
    docker image tag ghcr.io/cardano-foundation/moog/moog-agent:"$version" \
        "ghcr.io/cardano-foundation/moog/moog-agent:{{ tag }}"
    docker image tag ghcr.io/cardano-foundation/moog/moog-agent:"$version" \
        "ghcr.io/cardano-foundation/moog/moog-agent:latest"

build-moog tag='latest':
    #!/usr/bin/env bash
    nix build .#moog-docker-image
    docker load < result
    version=$(nix eval --raw .#version)
    docker image tag ghcr.io/cardano-foundation/moog/moog:"$version" \
        "ghcr.io/cardano-foundation/moog/moog:{{ tag }}"
    docker image tag ghcr.io/cardano-foundation/moog/moog:"$version" \
        "ghcr.io/cardano-foundation/moog/moog:latest"

build-moog-light tag='latest':
    #!/usr/bin/env bash
    nix build .#moog-docker-light-image
    docker load < result
    version=$(nix eval --raw .#version)
    docker image tag ghcr.io/cardano-foundation/moog/moog-light:"$version" \
        "ghcr.io/cardano-foundation/moog/moog-light:{{ tag }}"
    docker image tag ghcr.io/cardano-foundation/moog/moog-light:"$version" \
        "ghcr.io/cardano-foundation/moog/moog-light:latest"

start-moog-agent bg="false":
    #!/usr/bin/env bash
    # shellcheck disable=SC2050
    if [[ '{{ bg }}' == "true" ]]; then
        docker compose -f CD/moog-agent/docker-compose.yaml up -d
    else
        docker compose -f CD/moog-agent/docker-compose.yaml up
    fi

build-and-start-moog-agent bg="false":
    #!/usr/bin/env bash
    just build-moog-agent
    just start-moog-agent "{{ bg }}"

logs-moog-agent:
    #!/usr/bin/env bash
    docker compose -f CD/moog-agent/docker-compose.yaml logs -ft

stop-moog-agent:
    #!/usr/bin/env bash
    docker compose -f CD/moog-agent/docker-compose.yaml down

build-moog-oracle tag='latest':
    #!/usr/bin/env bash
    nix build .#moog-oracle-docker-image
    docker load < result
    version=$(nix eval --raw .#version)
    docker image tag ghcr.io/cardano-foundation/moog/moog-oracle:"$version" \
        "ghcr.io/cardano-foundation/moog/moog-oracle:{{ tag }}"
    docker image tag ghcr.io/cardano-foundation/moog/moog-oracle:"$version" \
        "ghcr.io/cardano-foundation/moog/moog-oracle:latest"

start-moog-oracle bg="false":
    #!/usr/bin/env bash
    # shellcheck disable=SC2050
    if [[ '{{ bg }}' == "true" ]]; then
        docker compose -f CD/moog-oracle/docker-compose.yaml up -d
    else
        docker compose -f CD/moog-oracle/docker-compose.yaml up
    fi

build-and-start-moog-oracle bg="false":
    #!/usr/bin/env bash
    just build-moog-oracle
    just start-moog-oracle "{{ bg }}"

logs-moog-oracle:
    #!/usr/bin/env bash
    docker compose -f CD/moog-oracle/docker-compose.yaml logs -ft

stop-moog-oracle:
    #!/usr/bin/env bash
    docker compose -f CD/moog-oracle/docker-compose.yaml down

recreate-moog-services:
    #!/usr/bin/env bash
    just build-and-start-moog-agent true
    just build-and-start-moog-oracle true

build-docker-images:
    #!/usr/bin/env bash
    tag=$(just moog-version)
    just build-moog "$tag"
    just build-moog-light "$tag"
    just build-moog-agent "$tag"
    just build-moog-oracle "$tag"

push-docker-images:
    #!/usr/bin/env bash
    tag=$(just moog-version)
    docker push "ghcr.io/cardano-foundation/moog/moog:$tag"
    docker push "ghcr.io/cardano-foundation/moog/moog:latest"
    docker push "ghcr.io/cardano-foundation/moog/moog-light:$tag"
    docker push "ghcr.io/cardano-foundation/moog/moog-light:latest"
    docker push "ghcr.io/cardano-foundation/moog/moog-agent:$tag"
    docker push "ghcr.io/cardano-foundation/moog/moog-oracle:$tag"
    docker push "ghcr.io/cardano-foundation/moog/moog-agent:latest"
    docker push "ghcr.io/cardano-foundation/moog/moog-oracle:latest"

cabal-version:
    #!/usr/bin/env bash
    nix eval --raw .#moog.version

moog-version:
    #!/usr/bin/env bash
    echo "v$(just cabal-version)"

release-linux:
    #!/usr/bin/env bash
    set -euo pipefail
    CI/release.sh "$(just moog-version)" linux64

release-macos:
    #!/usr/bin/env bash
    set -euo pipefail
    CI/release.sh "$(just moog-version)" darwin64
