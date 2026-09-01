# Plan — 101 readiness consumer

## Strategy

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
  branch (see `Q-001`).
- `gate.sh` is tracked on the baseline (see `Q-002`); this ticket's gate lives
  under the runtime root and does not touch it.

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

S1 leaves the canary red against the currently pinned offchain revision,
because that revision has no `/ready`. That is the contract working: the
dependency stays gated until the backend is current. It must not be "fixed".

## Verification

Ticket gate (runtime root, untracked): `cabal build all --enable-tests`,
`cabal test unit-tests`, `cabal-fmt -c`, `fourmolu -m check`, `hlint`,
`actionlint` on the touched workflow, plus the focused readiness spec.
`just CI` is not usable as a gate here: it requires `MOOG_GITHUB_PAT`,
`MOOG_SSH_PASSWORD` and wallet files, and runs the credentialed suites.
