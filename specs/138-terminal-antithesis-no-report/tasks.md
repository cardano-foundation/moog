# Tasks — #138

## Slice 1 — finish terminal no-report runs

- [X] T138-S1 RED: add StateSpec cases for `incomplete`+no-report and
  `cancelled`+no-report → `RunningFinish … OutcomeFailure`
  `antithesis://runs/<id>/no-triage-report`; rename the
  `completed`+no-report case to state the conservative policy and assert
  `RunningWait`.
- [X] T138-S1 GREEN: `runningDecision` terminal-no-report policy +
  deterministic synthetic-URL builder in `State.hs`.
- [X] T138-S1 surface result URL in `Process.hs` publish log (FR7).
- [X] T138-S1 gate green: `nix build .#moog-agent .#unit-tests`,
  `just unit`, `fourmolu -m check`, `hlint`.

## Slice 2 — live-derived regression (verification)

- [X] T138-S2 capture real /api/v0/runs payload for the stuck Leios run
  (incomplete, no `links` key) as `test/data/138-leios-incomplete-no-report.json`.
- [X] T138-S2 StateSpec test drives the live fixture through `parseRunsPage` +
  `runningDecision`, asserting `RunningFinish OutcomeFailure
  antithesis://runs/4720e2cc…-54-7/no-triage-report`; also proves the
  reconstructed TestRun hashes to the on-chain testRunId 42030238….
