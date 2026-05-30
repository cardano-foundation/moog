# Spec — Finish terminal Antithesis runs without triage reports (#138)

## P1 User Story

As a MOOG operator, I run `moog-agent` against Antithesis API runs that
reached a terminal off-chain state without a triage-report link, and I
observe the on-chain test-run leave `accepted`/`running` state with an
explicit failure attestation instead of remaining stuck forever.

## Problem

After #128 / #133, `moog-agent` reads Antithesis run state from
`/api/v0/runs` but treats terminal API runs without `links.triage_report`
as if they were still running. `runningDecision`
(`src/User/Agent/Antithesis/State.hs`) only returns `RunningFinish` when
*both* a terminal outcome *and* a report URL are present; every other case
falls through to `RunningWait`, so a terminal-but-report-less run logs
"still running according to Antithesis API" forever.

Live evidence (2026-05-30): Antithesis run `4720e2cc…-54-7`,
`status: incomplete`, `completed_at: 2026-05-27T17:08:46Z`, `links: {}`,
on-chain test-run `42030238…` still `accepted`.

## User Stories

- **US1** — A terminal failure-class run (`incomplete`, `cancelled`)
  without a report finishes on-chain as a failure, with a deterministic,
  audit-friendly synthetic URL.
- **US2** — A `completed` run without a report stays conservative (keeps
  waiting); success is never attested without the report link.
- **US3** — Agent logs no longer claim "still running" for terminal
  no-report runs.

## Functional Requirements

- **FR1** — `runningDecision` maps a single matching run with
  `status ∈ {incomplete, cancelled}` and **no** `triage_report` to
  `RunningFinish run OutcomeFailure (URL "antithesis://runs/<run_id>/no-triage-report")`.
- **FR2** — `runningDecision` maps a single matching run with
  `status = completed` and **no** `triage_report` to `RunningWait`
  (unchanged; documented as the conservative success policy).
- **FR3** — Non-terminal statuses (`starting`, `in_progress`, `unknown`)
  without a report keep returning `RunningWait`.
- **FR4** — The with-report behaviour from #133 is preserved unchanged.
- **FR5** — Duplicate-match runs keep returning `RunningDuplicate`
  (fail-closed) regardless of report presence.
- **FR6** — The synthetic URL is deterministic and depends only on the
  Antithesis `run_id`.
- **FR7** — The publish log line surfaces the result URL so terminal
  no-report finishes are distinguishable from report-backed finishes and
  from genuinely running runs.

## Success Criteria

- Regression test: `incomplete` + no report → `RunningFinish … OutcomeFailure …/no-triage-report`.
- Regression test: `cancelled` + no report → `RunningFinish … OutcomeFailure …/no-triage-report`.
- Regression test: `completed` + no report → `RunningWait` (named to state the policy).
- `just unit`, `fourmolu -m check`, `hlint`, and `nix build .#moog-agent .#unit-tests` all green.
- The Leios live case would reconcile from `accepted` to a terminal
  on-chain failure without an email.

## Non-goals

- No Antithesis stop/cancel/delete support.
- No email parsing for result synchronization.
- No finishing of non-terminal API states.
- No change to duplicate-run handling.
