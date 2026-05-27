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

### Stable-path layout for rotation

The agent's secrets directory uses a stable-path layout so that rotating credentials, the wallet, or tenant-specific configuration never requires editing the compose file. The compose file always mounts `/secrets/moog-agent/current/...`; `current` is a symlink that the operator points at the version currently in use.

```text
/secrets/moog-agent/
├── current -> new/                      # symlink: active version
├── new/
│   ├── agent.json                       # wallet JSON
│   ├── secrets.yaml                     # runtime settings + secrets
│   └── docker/
│       └── config.json                  # Docker registry credentials
└── old/
    ├── agent.json
    ├── secrets.yaml
    └── docker/
        └── config.json
```

Create the layout once at install time:

```bash
sudo mkdir -p /secrets/moog-agent/old/docker /secrets/moog-agent/new/docker
sudo ln -sfn new /secrets/moog-agent/current
```

Then populate `/secrets/moog-agent/new/` with the three files described below. Subsequent rotations alternate which side is "live" — see [Rotation](#rotation) below.

**`/secrets/moog-agent/current/agent.json`** — the agent wallet file (from `moog wallet create`).

**`/secrets/moog-agent/current/docker/config.json`** — Docker registry credentials for pulling/pushing Antithesis images.

**`/secrets/moog-agent/current/secrets.yaml`** — runtime settings and secrets together. The full key reference lives in [Secrets management](../user/secrets-management.md#all-supported-keys); a typical agent file looks like:

```yaml
# --- Runtime settings (file-backed) ---
tokenId: <moog-token-asset-id>
mpfsHost: https://mpfs.plutimus.com
walletFile: /run/secrets/agent-wallet
wait: 240
mpfsTimeoutSeconds: 120
pollIntervalSeconds: 60
minutes: 1440
registry: us-central1-docker.pkg.dev/<project>/<tenant-repository>
antithesisUser: cardano
antithesisLaunchUrl: https://amaru-cardano.antithesis.com/api/v1/launch/cardano

# --- Secrets ---
agentEmail: agent@example.com
agentEmailPassword: xxxx-xxxx-xxxx-xxxx     # Google app password
githubPAT: ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
antithesisPassword: your_antithesis_password
walletPassphrase: your_wallet_passphrase    # if wallet is encrypted
slackWebhook: https://hooks.slack.com/services/...   # optional
trustedRequesters:                                    # optional, ignored under --trust-all-requesters
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
      - MOOG_SECRETS_FILE=/run/secrets/secrets
      - DOCKER_CONFIG=/run/secrets/docker
    restart: always
    privileged: true
    command: "--trust-all-requesters"
    volumes:
      - tmp:/tmp
      - /etc/ssl/certs/ca-certificates.crt:/etc/ssl/certs/ca-certificates.crt:ro
      - /var/run/docker.sock:/var/run/docker.sock:rw
secrets:
  agent-wallet:
    file: /secrets/moog-agent/current/agent.json
  secrets:
    file: /secrets/moog-agent/current/secrets.yaml
  docker:
    file: /secrets/moog-agent/current/docker/config.json
volumes:
  tmp:
```

All tenant-specific values (`tokenId`, `mpfsHost`, `registry`, `antithesisUser`, `antithesisLaunchUrl`, poll/timeout tunables) live in `secrets.yaml` and are rotated with it; the compose file itself is identical across tenants.

!!! warning "Privileged mode and Docker socket"
    The agent container requires privileged mode and access to the Docker socket because it runs `docker compose` to validate test assets locally before pushing them to Antithesis. See [Security](security.md#docker-security) for implications.

### Start and verify

```bash
export MOOG_VERSION=<desired-version>
docker compose up -d
docker compose logs -f moog-agent
```

The agent polls for pending test runs every `--poll-interval` seconds (default 60) and checks for email results within the `--minutes` window.

### Rotation

To rotate credentials, the wallet, or any tenant-specific runtime setting (`tokenId`, `antithesisLaunchUrl`, `registry`, …), prepare the new files alongside the current ones and flip the `current` symlink — the compose file never changes.

1. Populate the inactive side (`new/` if `current` points at `old/`, and vice versa):
   ```bash
   sudo cp /path/to/new-secrets.yaml /secrets/moog-agent/new/secrets.yaml
   sudo cp /path/to/new-agent.json   /secrets/moog-agent/new/agent.json
   sudo cp /path/to/new-config.json  /secrets/moog-agent/new/docker/config.json
   ```
2. Atomically repoint `current`:
   ```bash
   sudo ln -sfn new /secrets/moog-agent/current
   ```
3. Re-create the container so it re-reads the symlink target:
   ```bash
   docker compose up -d --force-recreate moog-agent
   ```
4. To roll back, repoint `current` at `old/` and run the same `--force-recreate` command.

!!! warning "`restart` is not enough"
    Docker materialises swarm-style `secrets:` entries into a tmpfs at container *create* time, not at start time. `docker compose restart moog-agent` reuses the existing container, which still carries the *previous* symlink target. **You must `docker compose up -d --force-recreate moog-agent`** (or `down`/`up`) to pick up the new files. Operators have repeatedly tripped on this: a rotation appears to "do nothing" until the next full recreate.

## Environment Variables Reference

| Variable | Used by | Description |
|---|---|---|
| `MOOG_MPFS_HOST` | oracle, agent | URL of the MPFS service |
| `MOOG_WALLET_FILE` | oracle, agent | Path to the wallet JSON file |
| `MOOG_TOKEN_ID` | oracle, agent | The moog token asset ID on Cardano |
| `MOOG_SECRETS_FILE` | oracle, agent | Path to `secrets.yaml` |
| `MOOG_WAIT` | oracle, agent | Seconds between polling cycles |
| `MOOG_ANTITHESIS_USER` | agent | Antithesis platform username |
| `MOOG_ANTITHESIS_LAUNCH_URL` | agent | Antithesis tenant launch URL (alternative to `antithesisLaunchUrl` in `secrets.yaml`) |
| `MOOG_REGISTRY` | agent | URL of the registry where to push the config image (alternative to `--registry`) |
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

For changes to environment variables in `docker-compose.yaml`, restart the service:

```bash
docker compose restart
```

For changes to `secrets.yaml`, the wallet file, or `docker/config.json` on the agent, follow the [stable-path rotation procedure](#rotation) above — `docker compose restart` is **not** enough because the secrets tmpfs is captured at container create time. The oracle has the same caveat: update its files in place and run `docker compose up -d --force-recreate moog-oracle`.

No state is lost across rotation or recreate — both services resume polling from the current on-chain state.
