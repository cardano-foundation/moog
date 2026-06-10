# #177 — Repoint integration + e2e harness to the new facts-only MPFS

## P1 user story

As a maintainer, I run the moog integration + e2e suites on `moog-v2` against the
**new** facts-only offchain MPFS (not the legacy `mpfs:v1.1.0`), and the harness boots a
funded token successfully.

## Context

The legacy harness (`test-E2E/fixtures/docker-compose.yml`) launches two containers:
`yaci-cli` (a local devnet + faucet) and `ghcr.io/cardano-foundation/mpfs/mpfs:v1.1.0`
(the legacy MPFS, talking to yaci via `--provider yaci --yaci-store-host …`). The
integration-tests workflow then waits on `$MPFS_HOST/tokens .indexerStatus.ready`, funds
the wallet via yaci's `/local-cluster/api/addresses/topup`, and runs `nix run .#integration-tests`.

The new offchain MPFS server (`mpfs-serve`) no longer speaks the yaci/provider protocol —
it wants a cardano-node socket, RocksDB, genesis JSON, and a cage blueprint. The offchain
ships a self-contained wrapper, **`mpfs-devnet-server`**, that boots its own single-node
devnet internally, configured by `MPFS_BLUEPRINT` + `E2E_GENESIS_DIR`, serving the facts
API on `--port`. The moog **#152 canary already drives this server end-to-end**, including
**funding its wallet by patching `shelley-genesis.json` `initialFunds`** (the canary's
`fundedGenesis` / `patchInitialFunds` / `withDevnetServer`).

## Acceptance criteria

- [ ] `test-E2E/fixtures/docker-compose.yml`'s legacy `mpfs` service (and the now-unused
      `yaci-cli` service, unless still needed for chain queries) is removed; the harness
      stands up the new offchain `mpfs-devnet-server` instead.
- [ ] The harness pre-funds the integration test wallet(s) at genesis (reusing the
      canary's genesis-patching), replacing the yaci topup step.
- [ ] `nix run .#integration-tests` reaches a booted, funded token against the new MPFS
      (`$MPFS_HOST/tokens .indexerStatus.ready == true`), regardless of downstream
      oracle/agent flow breakage (those are #178/#179).
- [ ] The integration-tests + e2e-tests workflows trigger on `moog-v2` and run on a
      runner that can build the nix closure (self-hosted `[self-hosted, moog]`, mirroring
      the #152 canary — `ubuntu-latest` OOM'd on the canary).
- [ ] `cardano-mpfs-offchain` is pinned in moog's nix inputs at offchain `main`
      (`99cf2a2…`, matching `cabal.project`) as the source of `mpfs-devnet-server`.

## Non-goals

- Fixing oracle service flow breakage against the new MPFS (#178).
- Fixing agent service flow breakage / mocking Antithesis (#179).
- Publishing an OCI image for the new server (option A — rejected; see issue #177).
- The #153 production-host cutover.

## Parent

#181. Serial foundation — #178 and #179 depend on this.
