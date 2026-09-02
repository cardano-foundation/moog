# 101 — moog-v2 becomes the real `/ready` consumer

Ticket: M2-T101. Contract consumed: C-SIGNALS. Contracts produced: C-CANARY,
C-DEP-PINS.
Baseline: `origin/moog-v2` @ `190048f7f7e47cd838c3fb5961ad10e4cc654091`.

## Problem

moog-v2 has no readiness contract with MPFS. It re-derives readiness three
different ways, each by reaching inside a producer **data** route:

| Site | What it waits for |
|---|---|
| `src/MPFS/Devnet.hs:348` `waitForStatus` | `GET /status` returns 200 |
| `test-integration/Main.hs:116` `waitForSnapshotReady` | `GET /status` body has non-null `utxo_root` |
| `test-E2E/Main.hs:133` `waitForSnapshotReady` | the same, **plus** `POST /facts/boot` whose body does not contain the string `Indexer syncing` |

All three are client-side reconstructions of a signal the server should
publish. The third matches on an error string — a contract nobody declared.

A-003 rules that a client may trust only readiness, and that readiness is
`/ready`. C-CANARY requires the canary to poll `/ready`, never `/status`, and
to prove the dependency stays gated until the backend is current.

### The second problem: moog cannot compile against a current producer

Ruling `A-002` (filed as `m2-t387` `Q-002`) hands this ticket C-DEP-PINS.
Measured, with a treatment/control pair over disposable moog-v2 clones that
differ only in the offchain revision: moog-v2 fails to compile against offchain
`main` (`0f82465`) **and** against PR #387 (`495d123`), identically, on
`OnChainTokenState` not having field `stateStakeScript`.

The cause is not either offchain revision. moog declares its own
`cardano-mpfs-onchain` source-repository-package, and in the composed build
moog's pin wins:

| Declares `cardano-mpfs-onchain` | revision |
|---|---|
| moog `cabal.project:128` | `457c1cbc` |
| offchain `5f200647` (the revision moog pins) `cabal.project:96` | `457c1cbc` |
| offchain `main` / `#387` `cabal.project:103` | `39bf3302` |

So the pins agree **today** and disagree the moment the offchain pin moves
forward — which is exactly what this milestone is about to do. Every M2 unit
claiming a moog-v2 boundary proof is blocked behind that skew, not only #387.
A gate that catches it must therefore land before the pin moves, not after.

## User stories

- As the moog canary, I wait on the MPFS **readiness** signal, so that a boot
  attempt never races an indexer that has no current root.
- As a moog maintainer, I can see from a test that the readiness probe targets
  `/ready`, so that a future edit to `/status` cannot silently reintroduce the
  old coupling.
- As an operator, I can tell from moog's own docs that moog gates on `/ready`
  and never supervises MPFS, so that nobody wires readiness to autoheal.

## Requirements

- **R-101-1** The devnet readiness gate probes `GET /ready` and no other route.
- **R-101-2** The gate returns control only on HTTP 200 from `/ready`.
- **R-101-3** The gate reports how many not-ready observations it made before
  succeeding, and that count is carried into the canary's JSON result.
- **R-101-4** The canary fails when the not-ready count is zero, i.e. when it
  cannot show the gate actually held.
- **R-101-5** A route-absent backend (`404`) and a not-yet-current backend
  (`503`) produce **distinct** terminal failures, each naming its cause.
- **R-101-6** While `/ready` is not 200, the canary samples a data route and
  requires it to be non-200; a 200 there is a reported C-SIGNALS violation.
- **R-101-7** Server-process liveness stays a separate check from HTTP
  readiness: an exited server fails immediately, not after the readiness
  timeout.
- **R-101-8** moog adds no container healthcheck, supervisor probe, or autoheal
  configuration against MPFS, and records in its ops doc that `/live` is the
  only supervisor signal and `/ready` is the dependency gate.
- **R-101-9** moog's declared `cardano-mpfs-onchain` revision equals the
  revision declared by the offchain revision moog pins. The comparison reads
  both from their real sources; neither side may be a value transcribed into
  moog and left to rot.
- **R-101-10** That comparison is enforcing: it fails the build in CI and in
  the ticket gate, and it is proved able to fail against the known-skewed
  revision (`39bf3302`) rather than only observed passing.
- **R-101-11** After the revisions agree, the real composed consumer compiles
  and its unit suite passes. Revision equality is a necessary condition that is
  reported first because it names the cause; it never substitutes for the
  build.
- **R-101-12** moog moves to the accepted M2 producer revision and to the
  `cardano-mpfs-onchain` revision that producer declares. The producer SHA is
  **not** guessed ahead of M2-T275 acceptance.

## Rejection behaviour

- `/ready` absent (404 throughout) → fail naming an MPFS build that predates
  the C-SIGNALS contract. **This is the correct outcome, not a defect to
  soften:** running the canary against a non-conforming backend must stay red.
- `/ready` 503 throughout the budget → fail naming a backend that never became
  current, with the observed poll count.
- server process exits → fail immediately with the server log.
- a data route answers 200 while `/ready` is not 200 → fail naming the
  C-SIGNALS data-gating violation.
- moog's onchain revision differs from the one its pinned offchain revision
  declares → fail naming both revisions and both file positions, **before**
  spending a compile that would fail later with an unrelated-looking type
  error. The `stateStakeScript` error is what a skew looks like when nothing
  reports the skew.

## Observable success

- Unit suite drives the gate through scripted status sequences and observes
  each outcome above, with every assertion demonstrated able to fail.
- The canary, run against an M2-T275 producer build, completes with
  `readyNotReadyPolls >= 1` in its canonical JSON, proving it was gated.
- The pin check passes at the accepted baseline, fails against `39bf3302`, and
  both results are recorded as evidence.

## Non-goals

- Production URL or configuration cutover (project D2) — forbidden here.
- Merge, unfreeze, or `main`/Latest alignment (D3/D4).
- Any change to requester/oracle/agent or Antithesis test-run semantics.
- Publishing the deployment supervisor contract — that is C-SUPERVISOR, owned
  by M2-E-PUBLISH. This ticket records only moog's own consumer stance.
