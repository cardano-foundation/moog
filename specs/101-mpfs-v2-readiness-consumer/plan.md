# Plan — 101 readiness consumer

## Strategy

Two contracts, deliberately kept apart. C-DEP-PINS is a build-composition
contract and lands first as its own slice, because the pin skew it catches
blocks every M2 unit that claims a moog-v2 boundary and it must be enforcing
**before** the offchain pin moves forward. C-CANARY is the readiness contract
and is untouched by it.

Promote readiness out of the devnet lifecycle helper into its own module so the
contract has one owner and one testable surface, then move the canary onto it.
The two heavyweight suites migrate in a later slice, because migrating them
couples all of moog-v2's integration/E2E CI to the producer landing.

## Constraints

- Accepted base `origin/moog-v2` @ `190048f7`; frozen `main` is not a baseline.
- The producer (`/ready`) does not exist yet. Implementation may proceed against
  the accepted C-SIGNALS schema; **final integration proof waits for M2-T275's
  pushed producer branch.** Do not mock the final boundary to reach a green.
- `/code/moog-issue-275` is read-only. PR #101's own branch is not this lane's
  branch: per ruling `A-001`, this lane opens a **new** draft PR
  `feat/mpfs-v2-readiness-gate-canary-m2` -> `moog-v2` once immutable gates and
  accepted commits exist. PR #101 is not mutated, closed, retargeted, or
  commented on; it is recorded as superseded in the final evidence.
- The producer revision for the final pin move is **unknown until M2-T275 is
  accepted**. Specify and gate the alignment now; park the pin commit and its
  composed-build proof on the accepted producer (`R-101-12`). Guessing the SHA
  is forbidden.
- The offchain repository is public: the revision-comparison check can read a
  pinned revision's `cabal.project` unauthenticated (verified, HTTP 200 for
  `5f200647`). It needs no credential and must not acquire one.
- `gate.sh` is tracked on the baseline. Per ruling `A-002` there is **no
  waiver**: the legacy tracked gate is migrated in one orchestrator-owned
  cleanup slice (`S0`) that is mechanically separate from C-CANARY behaviour.
  After `S0` the ticket gate lives at `./gate.sh`, untracked and ignored, with
  the authoritative copy under the runtime root, and absent from every later
  commit.

## Live boundary

The diagnostic question — *what boundary does this exercise that the unit suite
cannot?* — has a non-empty answer: whether a real `mpfs-devnet-server` actually
returns 503 then 200 on `/ready`, and whether its data routes gate in step.

Coverage is split deliberately:

- **Unit** owns the gate's decision logic, driven through injected status
  sequences: gated/not-gated, 404 vs 503 vs timeout, liveness-vs-readiness.
- **Live boundary** owns the seam, via the `MPFS v2 Canary` workflow run
  against M2-T275's producer ref. This workflow is `workflow_dispatch`-only, so
  it is a **named operator follow-up** with a verifiable artifact (run URL plus
  the canary's canonical JSON), recorded in the PR body before the draft may be
  marked ready.

## Ordered slices

- **S0 — legacy gate migration.** *(Orchestrator-owned; not commit-owner work.)*
  Per `A-002`, untrack the baseline's root `gate.sh` (`git rm --cached gate.sh`),
  add a `/gate.sh` ignore rule, and install this ticket's frozen gate at
  `./gate.sh`. No production behaviour changes; it is prerequisite hygiene so the
  finalization audit's untracked-gate rule can hold, and it is explained as such
  in the PR body.
- **SD1 — dependency-pin alignment gate.** *(C-DEP-PINS; per `A-002` this
  ticket owns it.)* An enforcing check that reads moog's declared
  `cardano-mpfs-onchain` revision and the revision declared by the offchain
  revision moog pins, fails when they differ naming both, and then requires the
  real composed build. Wired into CI and the ticket gate. Green at the accepted
  baseline and proved red against `39bf3302`. Independent of the producer;
  landable today, and deliberately ahead of S1 so the skew cannot ride in
  behind a readiness change.
- **S1 — readiness gate and canary consumer.** New readiness module owning the
  probe path, the poll loop, the not-ready accounting, and the distinct failure
  vocabulary. `withDevnetServer` takes an explicit readiness policy; the canary
  selects the `/ready` policy while the two suites keep the legacy policy so the
  slice stays bisect-safe. Canary result gains the not-ready count and its
  assertion. Ops doc records moog's consumer stance and the no-supervisor fence.
  Independent of the producer; unit-provable today.
- **S2 — harness consolidation.** *(WAITING on M2-T275 + a cabal.project pin
  carrying `/ready`.)* Move `test-integration` and `test-E2E` onto the readiness
  policy, delete both `waitForSnapshotReady` implementations and the legacy
  policy constructor, removing the last `/status`-as-readiness couplings and the
  `Indexer syncing` string match.
- **S3 — live-boundary integration proof.** *(WAITING on M2-T275.)* Dispatch the
  canary workflow with `offchain_ref` set to the producer branch; record run URL
  and JSON as the C-CANARY artifact.
- **SD2 — pin move to the accepted producer.** *(WAITING on M2-T275
  acceptance.)* Move moog's offchain pin to the accepted producer revision and
  its `cardano-mpfs-onchain` pin to the revision that producer declares, with
  both `--sha256` hashes refreshed. SD1's check is what proves the pair is
  consistent; the composed build and moog unit suite are what prove it works.
  This is also the rerun that closes `m2-t387`'s `INV-5`.

S1 leaves the canary red against the currently pinned offchain revision,
because that revision has no `/ready`. That is the contract working: the
dependency stays gated until the backend is current. It must not be "fixed".

## Verification

Two levels, because two slices are dispatched against the same tree.

**Ticket gate** `./gate.sh` — untracked and ignored after `S0`, authoritative
copy under the runtime root. It holds the legs every slice must satisfy:
`git diff --check`, `cabal-fmt -c`, `fourmolu -m check`, `hlint`,
`cabal build all --enable-tests`, `cabal test unit-tests`, and `actionlint` on
a touched workflow. It accumulates each accepted slice's focused leg as that
slice lands.

**Frozen slice gates**, under the runtime root, one per dispatched slice. Each
runs the ticket gate and then its own focused leg — `gate/slice-SD1.sh` for the
pin check, `gate/slice-S1.sh` for the readiness spec. They are separate because
a single gate carrying both focused legs would fail a perfect candidate for
either slice by construction.

Every leg is falsified per class before dispatch, not once overall: a gate
proved able to fail on a missing spec has been proved able to fail on missing
specs and on nothing else.

`just CI` is not usable as a gate here: it requires `MOOG_GITHUB_PAT`,
`MOOG_SSH_PASSWORD` and wallet files, and runs the credentialed suites.
