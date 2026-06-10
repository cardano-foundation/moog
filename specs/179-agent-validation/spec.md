# #179 — Validate the agent service against the new facts-only MPFS (mock Antithesis)

## P1 user story
As an operator, the agent takes accepted on-chain test-runs, looks up their status (a mock
Antithesis API in tests), and writes results back on-chain via the new facts-only MPFS —
proven end-to-end by the integration suite (self-hosting the offchain `mpfs-devnet-server`, #177).

## Context (from scoping)
The agent is **already on the v2 facts API**: reads via `mpfsGetTokenFacts`
(`src/User/Agent/Process.hs:557`, facts-native), writes via
`mpfsRequestUpdateFromFacts`/`mpfsRequestInsertFromFacts`/`mpfsRequestDeleteFromFacts`
(`src/User/Agent/Cli.hs:424,454,513`). No legacy MPFS surface remains. So this ticket is
**validation coverage**: prove the reconcile→report cycle against the new MPFS, with a mock
Antithesis. Fix any breakage surfaced.

Key facts:
- Reconcile loop: `agentProcess` (`src/User/Agent/Process.hs:298`); transitions
  `submitRunning` (Pending→Running, Accept) and `submitDone` (Running→Done, Report) at
  `Process.hs:581-626`, routed through `agentCmd` (`src/User/Agent/Cli.hs:198`).
- Antithesis client is **direct ClientM, no DI seam** (`src/User/Agent/Antithesis/Client.hs:80-111`).
  Mocking ⇒ a Warp test server, pattern already proven in
  `test/User/Agent/Antithesis/ClientSpec.hs:106-157`. Use the **lightweight** approach (fake
  `ClientEnv` → Warp server), NOT a typeclass refactor.
- Pure planning is already unit-tested (`Antithesis/Plan.hs`, `State.hs`); the gap is the
  **integration** reconcile→report cycle against a real MPFS.

## Acceptance criteria
- [ ] A reusable mock-Antithesis Warp server helper (factored from `ClientSpec`) serves a
      configurable run list, wired into the integration harness.
- [ ] Integration spec: **Pending→Running** — boot a token, register a Pending test-run on-chain
      (facts), mock Antithesis returns the matching run in-progress, drive the agent accept;
      assert a Running fact is written to the devnet MPFS.
- [ ] Integration spec: **Running→Done** — mock Antithesis returns the run completed, drive the
      agent report; assert a Done fact (correct outcome + requester-encrypted URL) on-chain.
- [ ] `nix run .#integration-tests` green on the self-hosted fleet (orchestrator-verified on PR).
- [ ] Any agent code path found broken against the new MPFS is fixed (none expected — verify).

## Non-goals
- Real Antithesis API (the manual smoke is #180). Oracle flows (#178). E2E harness (#186).
- A typeclass refactor of the Antithesis client (use the Warp-server mock seam).

## Parent
#181. Depends on #177 (merged). Parallel with #178 (disjoint: `src/User/Agent/**` +
`test/MockMPFS.hs` + the agent integration specs vs `src/Oracle/**`). Only shared touch is the
one-line spec-list addition in `test-integration/Main.hs`.
