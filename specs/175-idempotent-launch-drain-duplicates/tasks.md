# Tasks — #175 idempotent Antithesis launch + drain duplicate runs

One `## Slice` section per bisect-safe commit. `[X]` when the orchestrator
accepts the slice (amended into the slice commit alongside its `Tasks:` trailer).

## Slice 1 — drain duplicate runs (part 2)

- [X] T175-S1 — RED: rewrite `PlanSpec` duplicate cases (pending + running) from
  skip → drain, add running-canonical-non-terminal + pending/running-agree
  cases; add `StateSpec` `canonicalRun` + duplicate-decision cases. Watch them
  fail against the current skip behavior.
- [X] T175-S1 — GREEN(State): add `descriptionKey`, `canonicalRun`,
  `terminalResult`; new `PendingDecision`/`RunningDecision` drain constructors;
  drain logic in `pendingDecision`/`runningDecision`.
- [X] T175-S1 — GREEN(Plan): `PendingDrainDuplicate`, `RunningDrainFinish`,
  `RunningDrainWait` actions; map them in `planPending`/`planRunning`.
- [X] T175-S1 — GREEN(Process): drain executors (accept canonical / finish
  canonical / wait), with the duplicate-run ids surfaced in the log.
- [X] T175-S1 — Gate green (`./gate.sh` + `cabal check`); one bisect-safe commit
  `fix(agent): drain duplicate Antithesis runs instead of stalling (#175)` with
  `Tasks: T175-S1`.

## Slice 2 — idempotent launch marker (part 1)

- [X] T175-S2 — RED: `PlanSpec` — thread `mempty` marker arg through existing
  calls; add "already-marked ⇒ `PendingAwaitObservation`" and "unmarked ⇒
  `PendingLaunchOnly`" cases. Watch the await case fail (double-launch) first.
- [X] T175-S2 — GREEN(Plan): `Set Text` param on `planAgentPoll`,
  `PendingAwaitObservation` action, marker check in `planPending`.
- [X] T175-S2 — GREEN(Process): `launchPendingTest :: IO Bool`;
  `executePendingAction :: … -> IO (Maybe Text)`; manual-recursion loop
  threading the rebuilt-each-poll marker set; failed launch ⇒ not marked.
- [X] T175-S2 — Gate green (`./gate.sh` + `cabal check` + `nix build .#moog-agent`);
  one bisect-safe commit
  `fix(agent): make Antithesis launch idempotent via in-process marker (#175)`
  with `Tasks: T175-S2`.

## Slice 3 — prefer authoritative terminal run as canonical (prod finding)

- [X] T175-S3 — RED: update PlanSpec + StateSpec "wait when canonical not
  terminal" cases to the new finish-from-completed expectation; add the
  mixed-outcome regression (incomplete `run-a` + completed `run-b` ⇒ finish from
  `run-b` as success). Watch them fail against `min run_id`.
- [X] T175-S3 — GREEN: add `statusPriority`; rerank `canonicalRun` (highest
  status priority, tiebreak `run_id`). Same-status cases unchanged.
- [X] T175-S3 — Gate (`nix run .#unit-tests` + `nix build .#moog-agent`) green;
  one bisect-safe commit `fix(agent): prefer the completed run as drain canonical (#175)`
  with `Tasks: T175-S3`.
