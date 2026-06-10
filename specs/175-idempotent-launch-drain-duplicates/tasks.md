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

- [ ] T175-S2 — RED: `PlanSpec` — thread `mempty` marker arg through existing
  calls; add "already-marked ⇒ `PendingAwaitObservation`" and "unmarked ⇒
  `PendingLaunchOnly`" cases. Watch the await case fail (double-launch) first.
- [ ] T175-S2 — GREEN(Plan): `Set Text` param on `planAgentPoll`,
  `PendingAwaitObservation` action, marker check in `planPending`.
- [ ] T175-S2 — GREEN(Process): `launchPendingTest :: IO Bool`;
  `executePendingAction :: … -> IO (Maybe Text)`; manual-recursion loop
  threading the rebuilt-each-poll marker set; failed launch ⇒ not marked.
- [ ] T175-S2 — Gate green (`./gate.sh` + `cabal check` + `nix build .#moog-agent`);
  one bisect-safe commit
  `fix(agent): make Antithesis launch idempotent via in-process marker (#175)`
  with `Tasks: T175-S2`.
