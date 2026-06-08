---
name: antithesis-moog
description: MOOG CLI for Antithesis test management on Cardano. Setup using the release binary (never moog-head — they encode types differently), identity switching (requester/agent/oracle wallets via being_*), token state inspection, retracting stuck requests during an oracle crash-loop, cleaning stale "running" tests, how the agent reconciles on-chain test-runs against the Antithesis REST API (email is gone — incomplete maps to failure), and the multi-tenant Antithesis configuration introduced by https://github.com/cardano-foundation/moog/pull/93. Triggers — "moog", "moog-head", "MOOG CLI", "being_requester", "being_agent", "being_oracle", "moog token", "moog facts test-runs", "moog retract", "moog agent report-test", "moog wallet info", "UnknownUpdateValidationFailure", "oracle crash-loop", "stale running test", "Antithesis report URL", "incomplete maps to failure", "Antithesis API reconciliation", "amaru-cardano", "pragma tenant", "MOOG_ANTITHESIS_LAUNCH_URL", "MOOG_REGISTRY".
---

# MOOG Operations

## Setup — CRITICAL

**Always use the release binary, never `moog-head`.**

```bash
cd /code/moog
# Download the release binary (same as the GitHub Actions workflow uses).
# Latest release is v0.5.1.3; asset naming changed at v0.5.x — the portable,
# statically-linked CLI is now `moog-<ver>-x86_64-linux-musl.tar.gz` (it
# extracts a single `moog` binary; moog-agent / moog-oracle are separate assets).
curl -sL "https://github.com/cardano-foundation/moog/releases/download/v0.5.1.3/moog-0.5.1.3-x86_64-linux-musl.tar.gz" | tar xz -C ./tmp/
export PATH=/code/moog/tmp:$PATH
source tmp/prod-setup.sh
```

> **Asset naming history:** ≤ v0.4.1.2 used `moog-vX.Y.Z.W-linux64.tar.gz`;
> v0.5.x switched to per-arch names — `moog-X.Y.Z.W-x86_64-linux-musl.tar.gz`
> (musl = static/portable), plus `.AppImage`, `.deb`, `.rpm`, and aarch64
> variants. Always match what `.github/workflows/cardano-node.yaml` downloads.

**WARNING**: `moog-head` in `/code/moog/tmp/` is a dev build. It reports the same `--version` as the release but encodes Duration differently (`{"minutes":0}` instead of `0`), producing requests the oracle can't parse (`UnknownUpdateValidationFailure`). Never symlink `moog-head` as `moog`. Always download the release binary.

The workflow at `.github/workflows/cardano-node.yaml` downloads from `cardano-foundation/moog` releases — match that exactly.

## Active Antithesis tenant: `amaru-cardano` (2026-05-26 →)

Antithesis migrated Cardano testing from the `cardano` tenant (`cardano.antithesis.com` / user `cardano`) to `amaru-cardano` (`amaru-cardano.antithesis.com` / user `pragma`). The moog-agent binary at `851aac3-dirty` (= `v0.4.1.2`) **hardcodes** the old URL at `src/User/Agent/PushTest.hs:246`. The multi-tenant fix lives in https://github.com/cardano-foundation/moog/pull/93 (branch `feat/multi-tenant-antithesis`, head `e6157f6`); it reads `MOOG_ANTITHESIS_LAUNCH_URL` and `MOOG_REGISTRY` from env. Anything older than that PR cannot reach the new tenant; treat the deployed `image:` tag as part of the diagnosis.

**Working credentials matrix (verify with curl before rotating anything — see `feedback_split_config_test_outside.md`):**

| Field | Value | Source |
|---|---|---|
| Antithesis URL | `https://amaru-cardano.antithesis.com/api/v1/launch/amaru-cardano` | env `MOOG_ANTITHESIS_LAUNCH_URL` |
| Antithesis user | `pragma` | env `MOOG_ANTITHESIS_USER` |
| Antithesis password | per `secrets.yaml.antithesisPassword` | secret file |
| GAR registry | `us-central1-docker.pkg.dev/molten-verve-216720/cardano-repository` (unchanged at the migration) | env `MOOG_REGISTRY` |

**Direct API check (outside moog) — succeeds with the right user/pw, 403 otherwise:**

```bash
ssh agent 'PW=$(sudo awk "/^antithesisPassword:/{sub(/^antithesisPassword:[[:space:]]*\"?/,\"\");sub(/\"?[[:space:]]*$/,\"\");print}" /secrets/moog-agent/new/secrets.yaml) && \
  curl -sS -o /dev/null -w "%{http_code}\n" -u "pragma:$PW" \
    -X POST https://amaru-cardano.antithesis.com/api/v1/launch/amaru-cardano \
    -H "Content-Type: application/json" -d "{}"'
# 200 → creds OK; 403 → creds rejected (and same 403 with no -u, so 403 alone doesn't distinguish)
```

## Identities

| Role | Wallet | Set with |
|------|--------|----------|
| Requester | `tmp/requester.json` | `being_requester` |
| Agent | `tmp/agent.json` | `being_agent` |
| Oracle | `tmp/oracle.json` | `being_oracle` |

Check current identity: `being`

## Key commands

| Command | Purpose |
|---------|---------|
| `moog token --no-pretty` | Full token state with pending requests |
| `moog facts test-runs --whose cfhal` | List test runs (run as `being_requester` to auto-decrypt the Antithesis report URL in the `url` field) |
| `moog retract -w <wallet> -o <outref>` | Retract a request you own |
| `moog agent report-test -i <id> -w <wallet> --outcome unknown --duration 0 --url "reason"` | Force-finish a stale test (manual override) |
| `moog antithesis runs --no-pretty` | List the Antithesis runs the agent reconciles against (see "How the agent reconciles results") |
| `moog wallet info` | Show wallet owner hash |

## Reading Antithesis runs directly (`moog antithesis`, v0.5.1.0+)

`moog antithesis` (added in v0.5.1.0) reaches the Antithesis report data
through the Moog proxy — no browser, SSO cookie, or Playwright. All
subcommands emit newline-delimited JSON; pair with `--no-pretty | jq`.
Exit codes on `runs`: `0` success, `2` auth/SSO failure, `3` proxy/network
failure, `4` invalid JSON from proxy.

| Command | Purpose |
|---------|---------|
| `moog antithesis runs [--limit N] [--cursor C]` | List runs (paginated; `--limit` max 100). Each entry carries `run_id` (e.g. `46d4…-54-7`), `status` (`in_progress` / `completed` / `incomplete`), and a JSON `description` with the testRun `directory` (e.g. `testnets/cardano_node_governance`). |
| `moog antithesis run --run-id ID` | Full details of one run. For `incomplete` runs it includes `failure_moment {input_hash, vtime}`; for `completed` runs it carries `links.triage_report` (the report URL). |
| `moog antithesis properties --run-id ID` | Property pass/fail for a **completed** run (404 while still in progress). |
| `moog antithesis events --run-id ID [--q TEXT]` | Stream matching events as NDJSON; free-text substring via `--q`; upstream caps at 50 results. |
| `moog antithesis logs --run-id ID --input-hash H --vtime V` | Logs at a specific moment. Needs a real moment — `in_progress` runs 400 (`Invalid query parameters`); a degenerate `0/0` failure moment returns empty. |
| `moog antithesis build-logs --run-id ID` | Stream the config-image **build** logs as NDJSON (`{timestamp, text, stream}`). Works regardless of run status — the fallback when there's no usable failure moment. |

**Find a run, then read it** — e.g. the latest governance run's build logs:

```bash
# Locate the run id by directory (the banner from prod-setup.sh precedes the JSON,
# so select the JSON line explicitly):
moog antithesis runs --limit 100 --no-pretty | grep '^{"data"' \
  | jq -r '.data[] | select(.description|contains("governance"))
           | [.created_at, .status, .run_id] | @tsv' | head

# Moment logs need input_hash + vtime from `run`; build-logs need only the id:
moog antithesis build-logs --run-id 46d4033b0cbaec3715d6a7a77b4411cb-54-7 --no-pretty
```

## Retract stuck requests (oracle crash-loop fix)

```bash
# 1. List pending requests on token
moog token --no-pretty | jq -c '.requests[] | {outref: .request.outputRefId, owner: .request.owner}'

# 2. Match owner to wallet (check with: moog wallet info)

# 3. Retract each owned request
being_requester  # or being_agent
moog retract -w $MOOG_WALLET_FILE -o "<outref>"

# 4. Oracle picks up changes automatically (no restart needed in theory,
#    but restart if it's stuck in a crash-loop reading stale state)
```

## How the agent reconciles results (Antithesis API, v0.5.1.2+)

**The agent derives outcomes from the Antithesis REST API, not email.** The
service loop (commit `ca82717e`, shipped in v0.5.1.2+) lists Antithesis runs and
matches each to an on-chain test-run by its rendered `description`, then advances
the on-chain phase. Reconciliation rules live in
`src/User/Agent/Antithesis/State.hs`:

- **`pending` on-chain** → no matching Antithesis run yet ⇒ launch it; exactly one
  match ⇒ accept; many ⇒ flagged duplicate.
- **`accepted` (running) on-chain** → when the matched run reaches a *terminal* API
  status, write a `finished` fact with this mapping (`terminalOutcome`):

  | Antithesis status | on-chain outcome |
  |---|---|
  | `completed` | `success` — **only if** the run has a `triage_report` link; without it the agent keeps waiting (success is never attested without the report) |
  | `incomplete` | **`failure`** |
  | `cancelled` | `failure` |
  | `starting` / `in_progress` / `unknown` | keep waiting |

- **Terminal but no triage-report link** (`incomplete` / `cancelled`): finished as
  `failure` anyway, with a deterministic synthetic URL
  `antithesis://runs/<run_id>/no-triage-report` (`terminalNoReportOutcome`, fix
  `04a74a40` / issue #138) so these no longer stay `accepted` forever.

So **`incomplete` has no on-chain representation of its own — it is recorded as
`outcome: failure`** (the report URL if one exists, else the `no-triage-report`
synthetic URL). The on-chain `duration` field is the *requested* hours, not actual
runtime, so a truncated run is invisible once finished.

## Clean up stale "running" tests

With the v0.5.1.2+ loop, terminal Antithesis runs auto-finish, so a genuinely
stuck `accepted` fact now means either a **correlation miss** (the agent's
`description` match failed — e.g. the run was launched by a different binary) or
an agent running a **pre-v0.5.1.2** binary. The manual escape hatch is unchanged:

```bash
# 1. Find accepted (running) tests on-chain
moog facts test-runs --whose cfhal --no-pretty | jq -c '.[] | select(.value.phase == "accepted") | {id: .id[:16], commit: .key.commitId[:8], try: .key.try}'

# 2. Confirm against the Antithesis API what status the run actually reached
moog antithesis runs --limit 100 --no-pretty | grep '^{"data"' | jq -r '.data[] | [.status, .run_id] | @tsv'

# 3. Report as unknown to finish them (manual override)
being_agent
moog agent report-test -i <full-test-run-id> -w $MOOG_WALLET_FILE --outcome unknown --duration 0 --url "stale-uncorrelated-run"
```

## Infrastructure SSH

| Host | Service | Container |
|------|---------|-----------|
| `oracle` | moog-oracle v0.4.1.2 | `oracle-moog-oracle-1` |
| `agent` | moog-agent (release binary; the v0.5.1.2+ loop reconciles via the Antithesis API) | `agent-moog-agent-1` |

Compose lives at `/opt/hal/infrastructure/moog/agent/docker-compose.yaml` on the `agent` host. Edits go in there, not in `/home/paolino/hal/...` (which is a checkout, not the deployed copy).

## Secrets layout

### Agent host (`agent`)

```
/secrets/moog-agent/
├── old/                 # previous rotation generation (kept for rollback)
│   ├── secrets.yaml
│   └── docker/config.json
└── new/                 # currently mounted by compose
    ├── secrets.yaml     # githubPAT, antithesisPassword, trustedRequesters,
    │                    # mnemonics or encryptedMnemonics, walletPassphrase, slackWebhook,
    │                    # tokenId, mpfsHost, registry, antithesisUser, antithesisLaunchUrl,
    │                    # pollIntervalSeconds, minutes, wait
    └── docker/
        ├── config.json  # docker auth (auths block with base64 _json_key:<sa>)
        └── sa-key.json  # raw GCP service-account JSON (kept for regenerating config.json)

agent-wallet (a separate compose secret)
└── /secrets/moog-agent/new/agent.json (or wherever the agent-wallet.file points)
```

Compose: `/opt/hal/infrastructure/moog/agent/docker-compose.yaml`.

### Oracle host (`oracle`)

```
/home/paolino/secrets/moog/oracle/
├── oracle.json          # oracle wallet (mnemonics OR encryptedMnemonics)
└── secrets.yaml         # githubPAT (cfhal-owned, distinct from agent's PAT),
                         # walletPassphrase (if oracle.json is encrypted)
```

Compose: `/opt/hal/infrastructure/moog/oracle/docker-compose.yaml`. Container env points `MOOG_WALLET_FILE=/run/secrets/oracle-wallet` and `MOOG_SECRETS_FILE=/run/secrets/secrets` at the in-container tmpfs paths the compose materialises from the two host files.

**The oracle wallet CANNOT be rotated on a live token.** The on-chain token's `state.owner` is permanently bound to the wallet's pubkey-hash at mint time (`moog token state --no-pretty | jq .state.owner`). Rotating the oracle wallet means burning the token and reminting — which loses the entire test-run history. If the oracle mnemonics leak (as happened 2026-05-28 — see lessons), the only mitigations are (a) `moog wallet encrypt` so on-disk material isn't immediately usable without the passphrase, and (b) accepting that the preprod blast radius is limited to fraudulent inclusions on this token's history and drainable preprod ADA — no mainnet impact. Treat the oracle wallet file as if it were a mainnet hot key: never `cat`, never `head`, never `scp` it off the host.

`old/` and `new/` may have *different* `githubPAT` / `antithesisPassword` / docker `config.json` values. When something fails on auth, diff the two — a half-applied rotation is a real failure mode.

**`docker/config.json` is NOT the raw SA key.** It must be a real docker config:
```json
{
  "auths": {
    "us-central1-docker.pkg.dev": {
      "auth": "<base64 of '_json_key:<sa-json-on-one-line-or-with-real-newlines>'>"
    }
  }
}
```
Build it from the SA key with:
```bash
ssh agent 'AUTH=$(sudo bash -c "printf %s \"_json_key:\$(cat /secrets/moog-agent/new/docker/sa-key.json)\" | base64 -w0"); \
  sudo tee /secrets/moog-agent/new/docker/config.json >/dev/null <<EOF
{"auths":{"us-central1-docker.pkg.dev":{"auth":"$AUTH"}}}
EOF'
```
**A `docker restart` does NOT re-read compose secrets** — the secret tmpfs is materialised at create time. Use `docker compose up -d --force-recreate moog-agent` to pick up a changed source file.

## Secret rotation

**Universal rules:**
- Never put a secret in the Bash command literal — `read -rs VAR` in your interactive terminal, expand `$VAR`, then `unset VAR`. Never `echo "$VAR"` to verify, just `echo "${VAR:0:7}..."`.
- Never `cat` / `head` / `tail` a wallet JSON. Use `moog wallet info -w <file>` — it derives owner + address + pubkey without revealing mnemonics.
- `docker restart` does **not** re-read compose secrets. Always `docker compose up -d --force-recreate <service>` after editing a secret-source file.
- After rotation, probe the new value through the service path (curl the API, watch the container logs for one poll cycle) — don't just trust that the file was updated.

### GitHub PAT — agent (owner: `paolino`, scope `public_repo`)

Used by the agent to git-clone testnet repos at the commit specified in each test-run request. Lives in `agent:/secrets/moog-agent/new/secrets.yaml` as `githubPAT: "..."` (quoted), and locally in `/code/moog/tmp/prod-setup.sh` as `export MOOG_GITHUB_PAT=...`.

```bash
# Create new PAT at https://github.com/settings/tokens — classic, scope public_repo, set expiration.
# Then in your terminal:
read -rs NEWPAT && \
  sed -i "s|^export MOOG_GITHUB_PAT=.*|export MOOG_GITHUB_PAT=$NEWPAT|" /code/moog/tmp/prod-setup.sh && \
  ssh agent "sudo sed -i 's|^githubPAT:.*|githubPAT: \"$NEWPAT\"|' /secrets/moog-agent/new/secrets.yaml && cd /opt/hal/infrastructure/moog/agent && sudo docker compose up -d --force-recreate moog-agent" && \
  unset NEWPAT

# Verify: probe old PAT (from your transcript / scrollback) → should be 401.
# Probe new PAT (read from file, prefix-only echo) → should be 200.
PAT=$(awk -F= '/^export MOOG_GITHUB_PAT=/{gsub(/"/,"",$2);print $2;exit}' /code/moog/tmp/prod-setup.sh); \
  echo "prefix: ${PAT:0:7}..., status: $(curl -sS -o /dev/null -w '%{http_code}' -H "Authorization: token $PAT" https://api.github.com/user)"; \
  unset PAT

# Confirm propagation to the running container:
ssh agent 'sudo docker logs --since 60s agent-moog-agent-1 2>&1 | grep -iE "401|forbidden|Polling" | head'
```

Caveat: the value is briefly visible in `ps` on `agent` while sed runs (~hundreds of ms) because shell expansion happens locally before ssh sends the line. To keep it off the remote command line, pipe via stdin:

```bash
read -rs NEWPAT && \
  printf '%s\n' "$NEWPAT" | ssh agent 'sudo sh -c "P=\$(cat); sed -i \"s|^githubPAT:.*|githubPAT: \\\"\$P\\\"|\" /secrets/moog-agent/new/secrets.yaml && cd /opt/hal/infrastructure/moog/agent && docker compose up -d --force-recreate moog-agent"' && \
  unset NEWPAT
```

### GitHub PAT — oracle (owner: `cfhal`, distinct from agent's PAT)

Used by the oracle to validate the `cardano-foundation/<repo>/<directory>/<commit>` path exists at the moment of validation (per `Lib.GitHub`). Lives in `oracle:/home/paolino/secrets/moog/oracle/secrets.yaml` as `githubPAT: ghp_...` (**unquoted** — different sed pattern from agent).

```bash
# Requires access to the cfhal GitHub account (or coordinate with whoever has it).
# Create a new PAT, scope public_repo.
read -rs NEWPAT && \
  ssh oracle "sudo sed -i 's|^githubPAT:.*|githubPAT: $NEWPAT|' /home/paolino/secrets/moog/oracle/secrets.yaml && cd /opt/hal/infrastructure/moog/oracle && sudo docker compose up -d --force-recreate moog-oracle" && \
  unset NEWPAT

# Verify: same probe pattern as the agent, but read from the oracle file.
ssh oracle 'PAT=$(sudo awk "/^githubPAT:/{print \$2}" /home/paolino/secrets/moog/oracle/secrets.yaml); \
  echo "prefix: ${PAT:0:7}..., status: $(curl -sS -o /dev/null -w "%{http_code}" -H "Authorization: token $PAT" https://api.github.com/user)"; unset PAT'

# Watch one oracle poll cycle for KeyDoesNotExist / 401:
ssh oracle 'sudo docker logs --since 60s oracle-moog-oracle-1 2>&1 | grep -iE "401|forbidden|ValidationFailure|Polling" | head'
```

### Antithesis password (per-tenant)

Lives in `agent:/secrets/moog-agent/new/secrets.yaml` as `antithesisPassword: "..."`. Paired with `MOOG_ANTITHESIS_USER` + `MOOG_ANTITHESIS_LAUNCH_URL` in the compose `environment:` block — both halves must be consistent for the tenant (see "Active Antithesis tenant" section above). Half-rotated pairs 403 silently.

```bash
# Verify the new password against the tenant BEFORE swapping it in, per feedback_split_config_test_outside.md:
read -rs NEWPW && \
  STATUS=$(curl -sS -o /dev/null -w "%{http_code}" -u "pragma:$NEWPW" \
    -X POST https://amaru-cardano.antithesis.com/api/v1/launch/amaru-cardano \
    -H "Content-Type: application/json" -d "{}"); \
  [ "$STATUS" = "200" ] && echo "OK — proceeding to rotate" || { echo "REJECTED (status=$STATUS) — not rotating"; unset NEWPW; false; }

# Then rotate:
[ -n "$NEWPW" ] && ssh agent "sudo sed -i 's|^antithesisPassword:.*|antithesisPassword: \"$NEWPW\"|' /secrets/moog-agent/new/secrets.yaml && cd /opt/hal/infrastructure/moog/agent && sudo docker compose up -d --force-recreate moog-agent"; \
  unset NEWPW

# Watch for PostToAntithesisFailure 403:
ssh agent 'sudo docker logs --since 120s agent-moog-agent-1 2>&1 | grep -iE "PostToAntithesisFailure|403|forbidden" | head'
```

### GAR docker config (GCP SA key for pushing config images)

Lives in `agent:/secrets/moog-agent/new/docker/config.json` (the `auths` block, NOT the raw SA key — see "Secrets layout"). The raw SA key is kept alongside at `sa-key.json` so the docker config can be regenerated.

```bash
# 1. Update the raw SA key:
read -rs NEWSA && \
  printf '%s' "$NEWSA" | ssh agent 'sudo tee /secrets/moog-agent/new/docker/sa-key.json >/dev/null' && \
  unset NEWSA

# 2. Regenerate docker config.json from it (server-side, value never crosses the wire as argv):
ssh agent 'AUTH=$(sudo bash -c "printf %s \"_json_key:\$(cat /secrets/moog-agent/new/docker/sa-key.json)\" | base64 -w0"); \
  sudo tee /secrets/moog-agent/new/docker/config.json >/dev/null <<EOF
{"auths":{"us-central1-docker.pkg.dev":{"auth":"$AUTH"}}}
EOF'

# 3. Force-recreate:
ssh agent 'cd /opt/hal/infrastructure/moog/agent && sudo docker compose up -d --force-recreate moog-agent'

# 4. Verify: trigger a push by trusted-pushing a test through the agent, then watch for DockerPushFailure.
ssh agent 'sudo docker logs --since 120s agent-moog-agent-1 2>&1 | grep -iE "DockerPushFailure|denied|Unauthenticated" | head'
```

### Agent email password (legacy — no longer used)

The agent **no longer collects results by email** — the service loop reconciles
against the Antithesis REST API (see "How the agent reconciles results"). Any
`agentEmailPassword` / `agentEmail` still in `secrets.yaml` is vestigial; the
`collect-results-for` / `collect-all-results` CLI subcommands also remain but are
off the result path. Nothing to rotate here.

### Wallet rotation (agent)

The agent-wallet (`agent:/secrets/moog-agent/new/agent.json` or wherever `agent-wallet.file` in compose points) holds the agent's signing key. Unlike the oracle wallet, it is **not** bound on-chain — rotation is just generating a new mnemonic, registering its pubkey-hash as an authorised agent on the token (via the requester or current agent's update path), updating the compose secret, and force-recreating.

```bash
# Generate locally — never via ssh+cat:
moog wallet new -w /tmp/new-agent.json   # writes mnemonics in plaintext to /tmp
moog wallet encrypt -w /tmp/new-agent.json   # optional — adds walletPassphrase requirement
scp /tmp/new-agent.json agent:/tmp/  # over the encrypted channel
ssh agent 'sudo mv /tmp/new-agent.json /secrets/moog-agent/new/agent.json && cd /opt/hal/infrastructure/moog/agent && sudo docker compose up -d --force-recreate moog-agent'
shred -uz /tmp/new-agent.json
```

Then run the on-chain re-registration (the precise call depends on whether the token has a separate `register-agent` fact or whether the agent identity is implicit in being whitelisted; check `moog facts --no-pretty | jq '.[].key | select(.type | test("agent|role"))'` first).

### Wallet rotation (oracle) — NOT POSSIBLE on a live token

See "Secrets layout / Oracle host" above. The on-chain `state.owner` is bound at mint time. If the oracle key is compromised, mitigations are: encrypt the on-disk mnemonics (`moog wallet encrypt`), accept the preprod blast radius, and document the incident.

## Debugging result reconciliation

The agent derives outcomes from the Antithesis REST API, not email. To debug why
an `accepted` on-chain test-run isn't finishing, compare the two sides:

```bash
# What the agent sees from Antithesis (the status drives the outcome):
moog antithesis runs --limit 100 --no-pretty | grep '^{"data"' \
  | jq -r '.data[] | [.created_at, .status, .run_id] | @tsv' | head

# Watch one agent poll cycle reconcile on-chain state against the API:
ssh agent 'sudo docker logs --since 120s agent-moog-agent-1 2>&1 | grep -iE "Polling|reconcil|finish|launch|accept" | head'
```

The usual reason an `accepted` run never finishes is a **correlation miss**: the
agent matches an Antithesis run to an on-chain test-run by the rendered
`description` (`matchingRuns` in `Antithesis/State.hs`), so if the description
doesn't byte-match (e.g. a launch from a different binary), `runningDecision`
returns `RunningWait` forever. Confirm the run exists in `moog antithesis runs`
and that its `description` matches before force-finishing.

## Common issues

- **Oracle crash-loop**: Stuck/malformed requests cause batch submission failure at MPFS.hs:87. Retract the bad requests. Oracle auto-recovers once token is clean.
- **UnknownUpdateValidationFailure**: Request JSON format doesn't match oracle's parser. Usually caused by using `moog-head` instead of the release binary. Retract and resubmit with the release binary.
- **Stale running tests**: an `accepted` on-chain run never reaches `finished`. With the v0.5.1.2+ API loop terminal runs auto-finish (`incomplete`/`cancelled` → `failure`, even with no triage report, via a synthetic `antithesis://runs/<id>/no-triage-report` URL), so a stuck `accepted` means the agent can't correlate the run (description mismatch) or is running a pre-v0.5.1.2 binary. Verify with `moog antithesis runs`, then `report-test --outcome unknown` as the manual override.
- **`runMPFS` error at MPFS.hs:87**: Oracle uses `error` instead of `Either` — any MPFS submission failure crashes the process instead of skipping the bad request.
- **Agent stale state**: Agent re-reads from chain each poll cycle but may lag. Restart if needed after on-chain changes.
- **`DownloadAssetsValidationError (AssetValidationSourceFailure SourceDirFailureDirAbsent)`**: Looks like "GitHub directory missing" but actually any HTTP status from GitHub (401/403/5xx) gets mapped to "directory absent" by `Lib/GitHub.hs:204`. Real cause is usually an expired GitHub PAT. Curl-test the PAT against `https://api.github.com/user`; rotate `secrets.yaml.githubPAT` if 401.
- **`DockerPushFailure ... denied: Unauthenticated request`**: The docker config at `/secrets/moog-agent/new/docker/config.json` is either malformed (raw SA key, not an `auths` block) or stale. See "Secrets layout". Confirm with a direct push from inside the container before assuming GAR side broke: `sudo docker exec agent-moog-agent-1 env -i DOCKER_CONFIG=/run/secrets/docker docker push <some-test-tag>`.
- **`PostToAntithesisFailure ... error: 403`**: With the old `851aac3-dirty` binary, the launch URL is hardcoded to `cardano.antithesis.com/api/v1/launch/cardano`. After the tenant migration this 403s for every credential. You need the PR #93 binary AND `MOOG_ANTITHESIS_LAUNCH_URL` + `MOOG_ANTITHESIS_USER=pragma` in compose. Test the API directly with curl before rotating the password.

## Lessons learned

### 2026-04-18

1. **`moog-head` ≠ release binary** even when `--version` matches. Dev builds may encode types differently. Always use the downloaded release.
2. **Don't restart oracle preemptively** — verify it's actually stuck before acting.
3. **Verify before diagnosing** — check actual data (the Antithesis API via `moog antithesis runs`, on-chain state) before theorizing about root causes.
4. **Result source is the Antithesis API, not email** (v0.5.1.2+, commit `ca82717e`). The old email-collection path (`collect-all-results`, the `agent-email-dump-1` sidecar) is off the result path; the agent reconciles on-chain test-runs against `moog antithesis runs` and maps run status to outcome (`incomplete` → `failure`).
5. **Decrypting the report URL**: the `url` field in a `finished` test-run fact is encrypted for the requester. `being_requester && moog facts test-runs --whose cfhal` returns it already decrypted as a plain `https://<tenant>.antithesis.com/report/...` URL. No separate `decrypt` step needed — `moog wallet decrypt` only handles wallet mnemonics.

### 2026-05-26 (tenant migration from `cardano` to `amaru-cardano`)

1. **Don't assume a 403 means "wrong password".** `cardano.antithesis.com` and `amaru-cardano.antithesis.com` both return 403 for missing/wrong creds *and* for valid creds against the *wrong* tenant. The only working combo gives 200; all other curl shapes look identical. Sweep candidate `(host, user, /launch/<path>)` triples with the existing password before requesting a rotation.
2. **`SourceDirFailureDirAbsent` is a misleading name.** Lib/GitHub.hs:204 maps any HTTP error to `Right False`. An expired PAT looks like "directory missing". Always curl `https://api.github.com/user` with the PAT before believing the error.
3. **`docker restart` does NOT re-read compose secrets.** The secret tmpfs is set up at container create. To pick up a changed source file, `docker compose up -d --force-recreate <service>`.
4. **The agent runs docker with `env = Just [("DOCKER_CONFIG", ...)]`** — no PATH, no HOME. Reproduce its calls with `env -i DOCKER_CONFIG=/run/secrets/docker docker ...` from inside the container; if that succeeds and the agent loop still 401s, you're looking at a stale tmpfs (see #3), not a permission issue.
5. **Both halves of the configuration matter.** `secrets.yaml` carries `antithesisPassword`/`githubPAT`; docker-compose carries `MOOG_ANTITHESIS_USER`, `MOOG_ANTITHESIS_LAUNCH_URL`, `MOOG_REGISTRY` (post-PR-#93). A half-rotated pair (new password + old user) will 403 silently. See `feedback_split_config_test_outside.md` in auto-memory.
6. **Credentials leak in moog error logs.** `Lib/System.hs:30-33` dumps the full curl invocation (including `-u user:pass`) on non-zero exit. Issue https://github.com/cardano-foundation/moog/issues/98 tracks this. Until fixed, treat any `PostToAntithesisFailure` message in operator chat as a credential disclosure and rotate.

### 2026-05-28 (secret-handling discipline, live-verify of moog#108)

1. **Never put a secret in the Bash command literal.** `MOOG_GITHUB_PAT=ghp_actual_value moog ...` lands in the conversation transcript, in the tool's task-output files, and in bash history. Caught when I echoed paolino's `public_repo` PAT inline; we had to revoke + rotate. Use `read -rs VAR` in the interactive terminal and expand `$VAR` — the literal never appears in any command that gets logged. See `feedback_never_echo_secrets.md` in auto-memory and the "Secret rotation" section above.
2. **Never `cat` / `head` / `tail` a wallet JSON.** Same incident: I ran `head -c 200` on the oracle wallet to "check the format" and dumped the full 24-word mnemonic into the transcript. The oracle wallet **cannot be rotated on a live token** (the on-chain `state.owner` is permanently bound at mint), so this was a permanent leak — only preprod blast radius saved it. Use `moog wallet info -w <file>` instead: it derives owner + address + pubkey without revealing mnemonics.
3. **Live-boundary verification before merging a moog launch-payload change.** moog#108 added compose-label-driven `custom.*_exclusion` keys. Unit tests proved the wire shape compiles and serializes; only a live launch from the PR-branch binary proves the `amaru-cardano.nb2` notebook honours the keys. Recipe: build the linux-tarball artifact from the PR's CI run, scp to `agent`, stop the daemon, manually `moog requester create-test ...` from a local cfhal wallet against a labels-carrying commit on `cardano-foundation/cardano-node-antithesis`, then run the new `moog-agent` once to push it; restart the old daemon when done. See `feedback_live_boundary_before_merge.md`.
4. **The linux tarball is statically linked.** `nix build .#moog` produces a binary that depends on `/nix/store/...` and won't run on non-NixOS hosts. The CI `Build tarballs` workflow produces `linux-tarball` artifacts (`moog-*-linux64.tar.gz`) that are statically linked and portable. For ad-hoc operator binaries, grab the artifact from the PR's CI run via `gh run download <run-id> -R cardano-foundation/moog -n linux-tarball`.
5. **The dev shell on `main` is currently broken.** `cabal-fmt 0.1.12` (pinned by `flake.nix:indexState = 2026-02-17`) doesn't allow `base 4.21.1.0` from the GHC 9.12.3 bump (`e07e88fc` on 2026-05-26). `nix develop` fails; `nix run .#unit-tests` and the no-nix CI path both still work. Use `nix run .#unit-tests` as the local gate until the index-state is bumped (or cabal-fmt's upper bound relaxed).
6. **Token state's `state.owner` is the oracle identity check.** `moog token --no-pretty | jq .state.owner` should byte-equal `moog wallet info -w <oracle-wallet>.owner`. If they differ, the running daemon is signing with the wrong key.
