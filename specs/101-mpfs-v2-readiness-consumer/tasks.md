# Tasks — 101

## S0 — legacy gate migration (orchestrator-owned)

- [X] T101-S0-1 Untrack the baseline's root `gate.sh` (`git rm --cached
      gate.sh`), add the `/gate.sh` ignore rule, and install the frozen ticket
      gate at `./gate.sh`. Owned by the ticket orchestrator, not the commit
      owner; no production behaviour changes (`A-002`).

## S1 — readiness gate and canary consumer

- [ ] T101-S1-1 `MPFS.Readiness` owns the policy, observation vocabulary,
      outcome vocabulary, and the injected-effect gate loop (R-101-1, R-101-2,
      R-101-3, R-101-5, R-101-7).
- [ ] T101-S1-2 `MPFS.Devnet.withDevnetServer` takes a readiness policy and
      delegates the wait; `waitForStatus` is removed; process liveness stays a
      separate immediate failure (R-101-7).
- [ ] T101-S1-3 Canary result carries the not-ready count; the canary selects
      `GateOnReady` and asserts the count is at least one (R-101-3, R-101-4).
- [ ] T101-S1-4 Data-route sampling during the not-ready window, reported as a
      distinct C-SIGNALS violation (R-101-6).
- [ ] T101-S1-5 `test/MPFS/ReadinessSpec` drives every outcome through scripted
      sequences, each assertion demonstrated able to fail.
- [ ] T101-S1-6 `docs/ops/mpfs-v2-canary.md` records that moog gates on
      `/ready`, never on `/status`, and never supervises MPFS; no healthcheck or
      autoheal configuration is added anywhere in the repository (R-101-8).

## S2 — harness consolidation (WAITING on M2-T275)

- [ ] T101-S2-1 `test-integration` and `test-E2E` move to `GateOnReady`.
- [ ] T101-S2-2 Both `waitForSnapshotReady` implementations and the
      `Indexer syncing` string match are deleted.
- [ ] T101-S2-3 `GateOnStatusLegacy` is deleted; no `/status`-as-readiness call
      site remains.

## S3 — live-boundary integration proof (WAITING on M2-T275)

- [ ] T101-S3-1 Dispatch `MPFS v2 Canary` with `offchain_ref` set to the
      producer branch; record the run URL and canonical JSON as the C-CANARY
      artifact in the PR body.
- [ ] T101-S3-2 Confirm from that run that `readyNotReadyPolls >= 1`, i.e. the
      gate was observed holding against a real backend.
