# MPFS v2 Boot/End Boundary Canary

The canary proves the MPFS v2 live boundary needed to boot a cage token
and then end that same token through facts-only client construction.

It runs this path:

```text
wallet -> GET /status -> POST /facts/boot -> verifyBootFacts -> bootCageTx -> sign -> POST /tx/submit -> GET /tx/:id -> GET /tokens/:id -> GET /status -> POST /facts/end -> verifyEndFacts -> endCageTx -> sign -> POST /tx/submit -> GET /tx/:id -> GET /tokens/:id until 404
```

Required environment:

```bash
export MPFS_DEVNET_SERVER=/path/to/mpfs-devnet-server
export MPFS_BLUEPRINT=/path/to/mpfs/blueprint.json
export E2E_GENESIS_DIR=/path/to/devnet/genesis
export MPFS_DEVNET_PATH=/path/containing/cardano-node
export MOOG_CANARY_POLLS=240
```

Run:

```bash
nix run .#moog-mpfs-v2-canary
```

Successful output contains:

```json
{
  "boundary": "mpfs-v2-end",
  "bootTransactionObserved": true,
  "endTransactionObserved": true,
  "tokenObservedBeforeEnd": true,
  "tokenGoneObserved": true
}
```

This command is release evidence only for the MPFS v2 boot/end boundary.
It does not validate requester, oracle, agent, or Antithesis test-run
semantics.

The CI smoke for this boundary runs the dedicated Haskell executable
`moog-mpfs-v2-canary`. It expects the paired offchain devnet executable
and its Nix-provided runtime paths:

```bash
export MPFS_DEVNET_SERVER=/path/to/mpfs-devnet-server
export MPFS_BLUEPRINT=/path/to/mpfs/blueprint.json
export E2E_GENESIS_DIR=/path/to/devnet/genesis
export MPFS_DEVNET_PATH=/path/containing/cardano-node
nix run .#moog-mpfs-v2-canary
```

The executable creates a deterministic MOOG wallet, patches a temporary
copy of the devnet genesis to fund that wallet, starts
`mpfs-devnet-server`, and runs the boot-then-end canary path against the
live HTTP API.

## Full v2 facts flow demo

This cast shows the facts-only lifecycle against a local MPFS devnet:
wallet creation and genesis funding, fast token boot, a config request
applied by the oracle, an expired request rejected by the oracle, and a
fresh request rejected too early by the Phase-3 guard.

```asciinema-player
{ "file": "assets/video/v2-full-flow.cast" }
```
