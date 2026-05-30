# Plan — #138

## Tech stack

Haskell, GHC 9.12.3 via haskell.nix. No new dependencies. Change is
confined to the pure decision layer plus its single log site.

## Modules touched

- `src/User/Agent/Antithesis/State.hs` — `runningDecision`: add the
  terminal-no-report policy and a deterministic synthetic-URL builder.
- `src/User/Agent/Process.hs` — `executeRunningAction`: surface the
  result URL in the publish log line (FR7).
- `test/User/Agent/Antithesis/StateSpec.hs` — regression tests
  (RED first).

Downstream (`Plan.hs`, `Process.hs` finish path, `submitDone`) already
handle `RunningFinish` generically — no new constructors needed.

## Slices

Single bisect-safe slice (S1):

1. **RED** — add three `StateSpec` cases: `incomplete`+no-report and
   `cancelled`+no-report expect `RunningFinish … OutcomeFailure` with the
   `antithesis://runs/<id>/no-triage-report` URL; `completed`+no-report
   expects `RunningWait` (renamed to state the policy). Watch them fail.
2. **GREEN** — in `runningDecision`, when the run is terminal and the
   report is absent, branch on a `noReportOutcome` policy: `incomplete`/
   `cancelled` → `RunningFinish` with the synthetic URL; everything else
   → `RunningWait`. Add the log-line URL in `Process.hs`.
3. Gate: `nix build .#moog-agent .#unit-tests`, `just unit`,
   `fourmolu -m check`, `hlint`.

## Risk / verification note

The change is pure reconciliation logic, fully covered by `StateSpec`.
It does not touch the Antithesis launch payload, so the
live-boundary-before-merge rule for launch changes does not apply. A live
reconcile of the Leios run is a natural post-merge operator check, not a
gate blocker.
