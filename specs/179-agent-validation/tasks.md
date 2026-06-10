# Tasks — #179 (agent validation, mock Antithesis)

## Slice A — mock-Antithesis Warp helper + agent integration spec module
- [ ] T179-SA factor a reusable `withMockAntithesisServer` (from test/User/Agent/Antithesis/ClientSpec.hs) serving a configurable run list
- [ ] T179-SA add an agent integration spec module + wire it into test-integration/Main.hs (one-line)
- [ ] T179-SA proof: mock server boots + the (stub) spec runs under the integration harness

## Slice B — Pending→Running (Accept) integration test
- [ ] T179-SB boot token; register a Pending test-run on-chain (facts); mock Antithesis returns the run in-progress; drive the agent accept (submitRunning / planAgentPoll+execute)
- [ ] T179-SB assert a Running fact is written to the devnet MPFS
- [ ] T179-SB proof: spec passes against the self-hosted devnet (locally)

## Slice C — Running→Done (Report) integration test
- [ ] T179-SC mock Antithesis returns the run completed; drive the agent report (submitDone)
- [ ] T179-SC assert a Done fact on-chain with correct outcome + requester-encrypted URL
- [ ] T179-SC fix any agent breakage surfaced; proof: spec passes against the devnet (locally); orchestrator confirms green on PR CI
