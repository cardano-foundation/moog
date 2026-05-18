# MPFS v2 Boot Canary

The boot canary isolates the MPFS v2 live boundary from the old MOOG
requester, oracle, and agent semantics.

It runs this path:

```text
wallet -> GET /status -> POST /facts/boot -> verify -> bootCageTx -> sign -> submit -> poll /transaction -> poll /token/:id
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
