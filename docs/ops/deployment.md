# Deployment Guide

This guide covers deploying the oracle and agent services as Docker containers.

## Infrastructure Overview

```mermaid
graph TB
    subgraph Oracle Machine
        oracle[moog-oracle container]
        oracle-secrets[secrets.yaml<br/>oracle-wallet.json]
    end
    subgraph Agent Machine
        agent[moog-agent container]
        agent-secrets[secrets.yaml<br/>agent-wallet.json<br/>docker/config.json]
        docker[Docker socket]
    end
    subgraph External Services
        mpfs[MPFS Service]
        cardano[Cardano Blockchain<br/>preprod]
        github[GitHub API]
        antithesis[Antithesis Platform]
    end

    oracle --> mpfs
    oracle --> github
    agent --> mpfs
    agent --> antithesis
    agent --> docker
    mpfs --> cardano
    oracle-secrets -.-> oracle
    agent-secrets -.-> agent
```

## Prerequisites

- **Docker** and **Docker Compose** on each machine
- **Cardano wallets** for both oracle and agent roles (created with `moog wallet create`)
- **GitHub PAT** with `repo` scope for the oracle (used to validate requests against GitHub)
- **GitHub PAT** with `repo` scope for the agent (used to download test assets)
- **Antithesis credentials** for the agent (registry password, platform user)
- **MPFS service** accessible from both machines (default: `https://mpfs.plutimus.com`)
- **Moog token** already created by the oracle (`moog oracle token boot`)

## Oracle Deployment

### Secrets setup

Create the secrets directory:

```bash
mkdir -p /secrets/moog-oracle
```

**`/secrets/moog-oracle/oracle.json`** — the oracle wallet file (from `moog wallet create`).

**`/secrets/moog-oracle/secrets.yaml`**:

```yaml
githubPAT: ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
walletPassphrase: your_wallet_passphrase  # if wallet is encrypted
```

### Docker Compose

Use the compose file from `CD/moog-oracle/docker-compose.yaml`:

```yaml
services:
  moog-oracle:
    image: ghcr.io/cardano-foundation/moog/moog-oracle:${MOOG_VERSION:-latest}
    secrets:
      - oracle-wallet
      - secrets
    environment:
      - MOOG_MPFS_HOST=https://mpfs.plutimus.com
      - MOOG_WALLET_FILE=/run/secrets/oracle-wallet
      - MOOG_TOKEN_ID=<your-token-id>
      - MOOG_SECRETS_FILE=/run/secrets/secrets
      - MOOG_WAIT=240
    restart: always
    volumes:
      - tmp:/tmp
secrets:
  oracle-wallet:
    file: /secrets/moog-oracle/oracle.json
  secrets:
    file: /secrets/moog-oracle/secrets.yaml
volumes:
  tmp:
```

### Start and verify

```bash
export MOOG_VERSION=<desired-version>
docker compose up -d
docker compose logs -f moog-oracle
```

Look for log lines showing successful polling and validation cycles. The oracle polls every `MOOG_WAIT` seconds (default 240).

## Agent Deployment

### Secrets setup

Create the secrets directory:

```bash
mkdir -p /secrets/moog-agent/docker
```

**`/secrets/moog-agent/agent.json`** — the agent wallet file.

**`/secrets/moog-agent/docker/config.json`** — Docker registry credentials for pulling/pushing Antithesis images.

**`/secrets/moog-agent/secrets.yaml`**:

```yaml
agentEmail: agent@example.com
agentEmailPassword: xxxx-xxxx-xxxx-xxxx  # Google app password
githubPAT: ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
antithesisPassword: your_antithesis_password
slackWebhook: https://hooks.slack.com/services/...  # optional
trustedRequesters:  # optional, ignored when --trust-all-requesters is used
  - requester_pkh_1
  - requester_pkh_2
```

### Docker Compose

Use the compose file from `CD/moog-agent/docker-compose.yaml`:

```yaml
services:
  moog-agent:
    image: ghcr.io/cardano-foundation/moog/moog-agent:${MOOG_VERSION:-latest}
    secrets:
      - agent-wallet
      - secrets
      - source: docker
        target: /run/secrets/docker/config.json
    environment:
      - MOOG_MPFS_HOST=https://mpfs.plutimus.com
      - MOOG_WALLET_FILE=/run/secrets/agent-wallet
      - MOOG_TOKEN_ID=<your-token-id>
      - MOOG_SECRETS_FILE=/run/secrets/secrets
      - MOOG_WAIT=240
      - MOOG_ANTITHESIS_USER=cardano
      - DOCKER_CONFIG=/run/secrets/docker
    restart: always
    privileged: true
    command: "--poll-interval 60 --minutes 1440 --trust-all-requesters"
    volumes:
      - tmp:/tmp
      - /etc/ssl/certs/ca-certificates.crt:/etc/ssl/certs/ca-certificates.crt:ro
      - /var/run/docker.sock:/var/run/docker.sock:rw
secrets:
  agent-wallet:
    file: /secrets/moog-agent/agent.json
  secrets:
    file: /secrets/moog-agent/secrets.yaml
  docker:
    file: /secrets/moog-agent/docker/config.json
volumes:
  tmp:
```

!!! warning "Privileged mode and Docker socket"
    The agent container requires privileged mode and access to the Docker socket because it runs `docker compose` to validate test assets locally before pushing them to Antithesis. See [Security](security.md#docker-security) for implications.

### Start and verify

```bash
export MOOG_VERSION=<desired-version>
docker compose up -d
docker compose logs -f moog-agent
```

The agent polls for pending test runs every `--poll-interval` seconds (default 60) and checks for email results within the `--minutes` window.

## Environment Variables Reference

| Variable | Used by | Description |
|---|---|---|
| `MOOG_MPFS_HOST` | oracle, agent | URL of the MPFS service |
| `MOOG_WALLET_FILE` | oracle, agent | Path to the wallet JSON file |
| `MOOG_TOKEN_ID` | oracle, agent | The moog token asset ID on Cardano |
| `MOOG_SECRETS_FILE` | oracle, agent | Path to `secrets.yaml` |
| `MOOG_WAIT` | oracle, agent | Seconds between polling cycles |
| `MOOG_ANTITHESIS_USER` | agent | Antithesis platform username |
| `DOCKER_CONFIG` | agent | Path to Docker config directory (for private registries) |

## Service Lifecycle

```mermaid
sequenceDiagram
    participant O as Oracle
    participant M as MPFS
    participant GH as GitHub
    participant A as Agent
    participant AT as Antithesis

    loop Every MOOG_WAIT seconds
        O->>M: Query pending requests
        O->>GH: Validate requests (users, roles, repos)
        O->>M: Submit state update transaction
    end

    loop Every poll-interval seconds
        A->>M: Query pending test runs
        A->>GH: Download test assets
        A->>A: Local docker compose validation
        A->>AT: Push test to Antithesis
        A->>M: Report test acceptance
    end

    loop Every poll-interval seconds
        A->>A: Check email for results
        A->>M: Report test completion
    end
```

## PAT Management

GitHub Personal Access Tokens are required by both oracle and agent for GitHub API access.

### Scope requirements

- **`repo`** — needed to access repository contents, CODEOWNERS files, and user profile repos

### Rotation

PATs have an expiration date. When a PAT expires:

1. The oracle/agent will receive 401 errors from GitHub API
2. All validation requests will fail
3. The service will continue polling but accomplish nothing, burning rate limit on failed auth

**To rotate a PAT:**

1. Create a new PAT at [github.com/settings/tokens](https://github.com/settings/tokens)
2. Update `secrets.yaml` with the new token
3. Restart the service: `docker compose restart`

!!! tip "Set a calendar reminder"
    Create a reminder 1 week before PAT expiry to avoid service disruption.

### Rate limits

GitHub API has a per-user rate limit of **5,000 requests/hour**. This limit is shared across all PATs belonging to the same GitHub user. If both oracle and agent use PATs from the same user, they share the 5,000 request budget.

See [Troubleshooting](troubleshooting.md#pat-expired-or-rate-limited) for diagnosing PAT issues.

## Monitoring

### Log checking

```bash
# Oracle logs
docker compose logs --tail 50

# Agent logs
docker compose logs --tail 50
```

### Health verification

```bash
# Check pending requests
moog token | jq '.requests | length'

# Check system facts
moog facts test-runs pending --pretty

# Check GitHub rate limit
curl -s -H "Authorization: token <PAT>" \
  https://api.github.com/rate_limit | jq '.rate'
```

## Updating

### Image version bump

```bash
export MOOG_VERSION=<new-version>
docker compose pull
docker compose up -d
```

### Configuration changes

For changes to `secrets.yaml` or environment variables, restart the service:

```bash
docker compose restart
```

No state is lost on restart — the service resumes polling from the current on-chain state.
