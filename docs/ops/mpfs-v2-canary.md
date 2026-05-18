# MPFS v2 Boot Canary

The boot canary isolates the MPFS v2 live boundary from the old MOOG
requester, oracle, and agent semantics.

It runs this path:

```text
wallet -> GET /status -> POST /facts/boot -> verify -> bootCageTx -> sign -> POST /tx/submit -> GET /tx/:id -> GET /tokens/:id
```

Required environment:

```bash
export MOOG_MPFS_HOST=http://127.0.0.1:3000
export MOOG_MPFS_BLUEPRINT=/path/to/mpfs/blueprint.json
export MOOG_WALLET_FILE=/path/to/funded-wallet.json
export MOOG_CANARY_POLLS=240
```

Run:

```bash
moog canary boot
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

For local live-boundary proof against the paired MPFS v2 offchain
service, fund the MOOG wallet on the devnet before starting
`mpfs-devnet-server`. One workable setup is a temporary copy of the
`cardano-node-clients` devnet genesis selected with `E2E_GENESIS_DIR`,
with the wallet's raw Shelley address bytes added to
`initialFunds`.
