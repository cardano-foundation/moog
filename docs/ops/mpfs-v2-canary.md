# MPFS v2 Boot Canary

The boot canary isolates the MPFS v2 live boundary from the old MOOG
requester, oracle, and agent semantics.

It runs this path:

```text
wallet -> GET /status -> POST /facts/boot -> verify -> bootCageTx -> sign -> POST /tx/submit -> GET /tx/:id -> GET /tokens/:id
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
  "boundary": "mpfs-v2-boot",
  "transactionObserved": true,
  "tokenObserved": true
}
```

This command is release evidence only for the lower boot boundary. It
does not validate MOOG requester, oracle, agent, or Antithesis test-run
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
`mpfs-devnet-server`, and runs the boot canary path against the live HTTP
API.
