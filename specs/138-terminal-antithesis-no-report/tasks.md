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
