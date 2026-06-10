# Spec — Idempotent Antithesis launch + drain duplicate runs (#175)

## P1 User Story

As a MOOG operator, I run `moog-agent` against a pending test-run from a
trusted requester, and the agent launches the Antithesis run **exactly once**
and then drives the on-chain transition (`pending → accepted → finished`) to
completion — even when the just-launched run is not yet visible in the
Antithesis API on the next poll, and even for the ~15 already-wedged
test-runs that currently carry duplicate API runs.

## Problem

`moog-agent` splits "launch" from "accept" across separate polls and leaves
**no marker** when it launches:

- `PendingLaunchOnly` (`Process.hs:369`) POSTs to Antithesis but records
  nothing locally or on-chain.
- `PendingAcceptObserved` (`Process.hs:379`) accepts on-chain only **after**
  the run is observed via the Antithesis API.

The pure classifier `planAgentPoll` (`Plan.hs`) → `pendingDecision`
(`State.hs`) decides purely from `matchingRuns testRun runs`:
`[] → PendingLaunch`, `[run] → PendingAccept`, `many → PendingDuplicate`.
Because the launch leaves no marker, when the just-launched run has not yet
propagated into `/api/v0/runs` by the next poll (observed ~1 min gap vs the
poll interval), `matchingRuns` is still `[]` ⇒ the classifier re-selects
`PendingLaunchOnly` ⇒ a **second** POST. The following poll then sees two
matching runs ⇒ `PendingSkipDuplicate` (`Process.hs:401`) /
`RunningSkipDuplicate` (`Process.hs:453`), which **skip the transition
indefinitely**.

Result: the request never advances past `pending`; completed runs never
reach `finished` (outcome never recorded). It is epidemic — essentially
every recent request. Live evidence: `testRunId 9c95dbd9…` produced runs
`f2a848b1…-54-7` @16:46:07 and `6d66bd67…-54-7` @16:47:12 (~1 min apart) and
is stuck `pending`; logs show ~15 pending + many done test-runs wedged with
`… has duplicate Antithesis API runs …; skipping … transition`.

### Code facts that constrain the fix

- The reconciliation key is the **deterministic** `description = renderTestRun
  testRunId tr` (`State.hs:matchingRuns`) — one stable key per test-run.
- The launch is fire-and-forget `curl --fail -X POST` (`PushTest.hs`) and
  returns only stdout; it does **not** surface a structured Antithesis run id.
  So idempotency must be a **launch marker keyed by the description**, not a
  run id captured from the POST response.
- `AntithesisRun` exposes no creation timestamp (`run_id`, `status`,
  `description`, `triage_report` only); run_ids are not time-ordered.
- `planAgentPoll` is pure and unit-tested directly (`PlanSpec.hs`) — the
  natural home for both the idempotency state and the drain logic + their RED
  tests.

## User Stories

- **US1 (idempotent launch)** — A trusted pending test-run with no matching
  API run yet is launched **once**; on subsequent polls, while still
  unobserved, the agent waits instead of re-POSTing.
- **US2 (accept after observation)** — Once the launched run becomes visible
  in the API, the agent accepts it on-chain (unchanged observe-before-accept
  safety: a 2xx POST is never accepted on-chain until the run is actually
  observed).
- **US3 (drain pending duplicates)** — A pending test-run with **multiple**
  matching API runs picks one canonical run and proceeds to accept on-chain,
  instead of skipping forever.
- **US4 (drain running duplicates)** — A running test-run with **multiple**
  matching API runs picks the **same** canonical run and applies the normal
  terminal/finish decision to it, instead of skipping forever.
- **US5 (clears the backlog)** — The ~15 wedged pending and the many wedged
  done test-runs already carrying duplicates drain to completion on the next
  agent polls without operator intervention.
- **US6 (no extra compute, observable)** — The agent stops issuing the second
  POST, and still logs when duplicates are observed (so the 2× waste already
  spent and any future drift are visible in the agent log).

## Functional Requirements

- **FR1** — The agent carries an in-process **launch marker** set keyed by the
  test-run description. `PendingLaunchOnly` executes only when the test-run is
  **not** already marked launched **and** has `[]` matching API runs; on a
  successful launch the description is added to the marker set.
- **FR2** — When a pending test-run has `[]` matching API runs **and** is
  already marked launched, the plan yields a new "await observation" action
  that performs **no** POST and **no** on-chain write (and logs the wait).
- **FR3** — The marker for a description is cleared (or ignored) once that
  test-run is observed in the API (`matchingRuns` non-empty) or leaves the
  pending facts; the marker set does not grow without bound across polls.
- **FR4** — `pendingDecision` with **multiple** matching runs selects a
  **canonical** run by a deterministic rule and yields an accept-the-canonical
  action (drain), replacing the skip-forever behavior. The duplicate set is
  still surfaced in the log.
- **FR5** — `runningDecision` with **multiple** matching runs selects the
  **same** canonical run by the **same** deterministic rule and applies the
  existing single-run terminal/finish logic to it (finish if terminal with a
  result; wait otherwise), replacing the skip-forever behavior. The duplicate
  set is still surfaced in the log.
- **FR6** — The canonical-pick rule is **deterministic and stable across
  polls** so the pending-side and running-side drains agree on the same run
  (otherwise the transition can re-stall).
- **FR7** — Single-match and no-match behavior is unchanged: `[] → launch`
  (subject to FR1/FR2), `[run] → accept` / normal terminal decision.
- **FR8** — Untrusted-requester handling (`PendingSkipUntrusted`) and the
  observe-before-accept ordering are preserved.

## Locked design decisions (operator-approved @ specs checkpoint)

**D1 — Idempotency mechanism (part 1): in-process launch marker.** FR1–FR3
stand. The deliberate observe-before-accept 2-phase design is preserved; the
marker is per-process (lost on restart), and the rare restart-window duplicate
is self-healed by the drain (FR4/FR5). (Rejected: accept-on-chain atomically
with the POST — it would discard the safety net so a 2xx POST that silently
produces no run wedges the run as `accepted` with nothing to finish it.)

**D2 — Marker liveness: simple set, no TTL.** The agent marks a description as
launched **only on a successful launch** (so a *failed* launch — download /
build / push / non-2xx POST — leaves no marker and is retried next poll). A
2xx launch that never becomes API-observable stays marked and is **not**
re-launched within the process; the drain + an operator restart cover that
rare case. (Rejected: poll-count grace — extra constant + reintroduced
duplicate risk.)

**D3 — Canonical-run pick: prefer the authoritative terminal run, then
`run_id`.** *(Revised after the live prod test — see Slice 3.)* The original
`minimumBy run_id` is **semantically blind**: for a double-launched test whose
two runs disagree (`completed` vs `incomplete`), it could pick the aborted
`incomplete` run and record a wrong `failure`. Live prod (2026-06-10) hit this
on 4 mixed-outcome test-runs. The canonical pick now ranks by status authority
— `completed` (ran to completion, carries the result) > `incomplete` >
`cancelled` > non-terminal > unknown — tie-broken by `run_id` for determinism.
Still a single shared `canonicalRun` used by both drains (FR6 holds). The unit
tests originally used same-status duplicates, so they never exercised the mixed
case — Slice 3 adds that coverage. (Note: `created_at`/`started_at` **are**
exposed by `/api/v0/runs`; moog's `AntithesisRun` just doesn't parse them — a
possible finer tiebreak, out of scope here.)

## Success Criteria

- **Idempotency (pure, `PlanSpec`)** — Given `[]` matching runs and the
  test-run already in the launch-marker set, `planAgentPoll` yields the
  await-observation action (no second `PendingLaunchOnly`); given `[]` and
  **not** marked, it yields `PendingLaunchOnly`.
- **Pending drain (pure)** — `planAgentPoll` with two matching pending runs
  yields accept-the-canonical (deterministic pick), not a skip.
- **Running drain (pure)** — `planAgentPoll` with two matching running runs
  (both terminal-with-report) yields finish-the-canonical for the **same** run
  the pending side would pick; with non-terminal canonical it waits.
- **Stability** — A property/regression showing the pending-side and
  running-side canonical picks coincide for the same match set.
- **Regression preserved** — All existing `PlanSpec` / `StateSpec` cases for
  single-match launch, accept, finish, and the #138 no-report finishes still
  pass (duplicate cases updated from skip → drain).
- **Gate green** — `just build`, `just unit`, `fourmolu -m check`,
  `hlint`, `cabal check`, and the agent build all green locally before push.
- **Live outcome** — On the next agent polls, the wedged backlog (~15 pending
  + the duplicated done runs) drains to `accepted`/`finished` and no second
  POST is issued for a freshly-launched run.

## Out of scope

- Adding a creation-timestamp field to the Antithesis API client/parsing
  (only relevant if D3 picks the timestamp alternative).
- Cross-restart durable persistence of the launch marker (disk/on-chain).
- Cancelling/garbage-collecting the spurious duplicate Antithesis runs
  already created (they expire on Antithesis; we only stop creating new ones
  and stop letting them stall the on-chain transition).
- Any change to the email path (already removed) or to MPFS transaction
  semantics.
