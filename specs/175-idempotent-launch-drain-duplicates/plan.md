# Plan — #175 idempotent Antithesis launch + drain duplicate runs

## Tech stack

Haskell (GHC 9.x, `-O0` dev), cabal multi-package, fourmolu (70-col, leading
commas/arrows), hlint, hspec unit tests. Pure logic lives in
`src/User/Agent/Antithesis/{State,Plan}.hs`; the IO poll loop in
`src/User/Agent/Process.hs`. Pure planner is unit-tested directly in
`test/User/Agent/Antithesis/PlanSpec.hs` (+ `StateSpec.hs`).

## Shared design (both slices)

- **Match / marker key** — one helper, single source of truth:
  `descriptionKey :: TestRun -> Text = T.pack (renderTestRun (mkTestRunId tr) tr)`.
  `matchingRuns` is refactored to use it; the launch marker (S2) and the
  PlanSpec fixtures key off the same value.
- **Canonical pick** — `canonicalRun :: [AntithesisRun] -> AntithesisRun =
  minimumBy (comparing antithesisRunId)`. Used by **both** the pending and
  running drains so they always agree (spec FR6). Total only on non-empty
  input; only ever called from a non-empty `matches` branch.

## Slice order

Drain first (S1), idempotency second (S2). Rationale: S1 is self-contained
(no change to `planAgentPoll`'s arity, no loop surgery), clears the wedged
backlog immediately, and keeps the two slices cleanly separated — S1 never
touches the marker/loop, S2 never touches duplicate logic. Each is one
bisect-safe commit with its own RED→GREEN in `PlanSpec`.

---

## Slice 1 — drain duplicate runs (part 2)

**Goal:** duplicate API matches stop skipping forever; pick the canonical run
and proceed (accept on the pending side, finish/wait on the running side).

### State.hs

- Export + add `descriptionKey :: TestRun -> Text`; refactor `matchingRuns` to
  use it.
- Export + add `canonicalRun :: [AntithesisRun] -> AntithesisRun`
  (`minimumBy (comparing antithesisRunId)`; `import Data.List (minimumBy)`,
  `import Data.Ord (comparing)`).
- Factor the single-run terminal logic into
  `terminalResult :: AntithesisRun -> Maybe (Outcome, URL)` (covers the
  with-report and no-report #138 cases). Reuse it for single and canonical.
- New decision constructors:
  ```haskell
  data PendingDecision
      = PendingLaunch
      | PendingAccept AntithesisRun
      | PendingAcceptDuplicate AntithesisRun [AntithesisRun]  -- canonical + all matches

  data RunningDecision
      = RunningWait
      | RunningFinish AntithesisRun Outcome URL
      | RunningFinishDuplicate AntithesisRun Outcome URL [AntithesisRun]  -- canonical terminal + all
      | RunningWaitDuplicate AntithesisRun [AntithesisRun]                -- canonical non-terminal + all
  ```
- Logic:
  ```haskell
  pendingDecision tr runs = case matchingRuns tr runs of
      []      -> PendingLaunch
      [run]   -> PendingAccept run
      matches -> PendingAcceptDuplicate (canonicalRun matches) matches

  runningDecision tr runs = case matchingRuns tr runs of
      []      -> RunningWait
      [run]   -> maybe RunningWait (uncurry (RunningFinish run)) (terminalResult run)
      matches -> let c = canonicalRun matches
                 in maybe (RunningWaitDuplicate c matches)
                          (\(o,u) -> RunningFinishDuplicate c o u matches)
                          (terminalResult c)
  ```

### Plan.hs

- Replace `PendingSkipDuplicate` →
  `PendingDrainDuplicate (Fact ...) AntithesisRun [AntithesisRun]`.
- Replace `RunningSkipDuplicate` with two drain actions:
  `RunningDrainFinish (Fact ...) AntithesisRun Outcome URL [AntithesisRun]`
  and `RunningDrainWait (Fact ...) AntithesisRun [AntithesisRun]`.
- `planPending` / `planRunning` map the new `*Decision` constructors to these
  actions (canonical + dup list flow straight through).

### Process.hs

- `executePendingAction`:
  - `PendingDrainDuplicate fact canonical dups` — log
    `"… has duplicate Antithesis API runs <ids>; draining: accepting canonical
    run <canonicalId>."` then `submitRunning opts testId` (same accept path as
    `PendingAcceptObserved`; on-chain accept does not reference a run id).
- `executeRunningAction`:
  - `RunningDrainFinish (Fact tr st _) canonical outcome url dups` — log drain +
    `submitDone opts testId (testRunDuration st) outcome url` (same finish path
    as `RunningFinishObserved`, with canonical's outcome/url).
  - `RunningDrainWait fact canonical dups` — log
    `"… duplicate runs <ids>; canonical <id> not yet terminal, waiting."`
    (no on-chain action).

### Tests (RED first)

- `PlanSpec`: rewrite the two duplicate cases from skip → drain:
  - pending dup `[runA "run-a", runB "run-b"]` ⇒
    `PendingDrainDuplicate pendingFact runA [runA, runB]` (canonical = `run-a`).
  - running dup (both `RunCompleted` + report) ⇒
    `RunningDrainFinish runningFact runA OutcomeSuccess (URL "…/run-a") [runA, runB]`.
  - new: running dup where canonical is non-terminal (`runA RunInProgress`
    no-report, `runB RunCompleted` report) ⇒ `RunningDrainWait runningFact runA
    [runA, runB]` — proves we track the canonical, not "any finished dup".
  - new: pending-side and running-side canonical coincide for an unordered
    match list `[runB, runA]` (both pick `run-a`).
- `StateSpec`: focused `canonicalRun` unit + `pendingDecision`/`runningDecision`
  duplicate cases.
- Preserve all existing single-match / no-report (#138) cases unchanged.

---

## Slice 2 — idempotent launch marker (part 1)

**Goal:** a freshly-launched run that is not yet API-visible is not POSTed a
second time.

### Plan.hs

- `planAgentPoll` gains a `Set Text` parameter (launched-but-unobserved
  description keys), inserted **after** `allowRequester`
  (`import Data.Set (Set)`, `qualified Data.Set as Set`).
- New action `PendingAwaitObservation (Fact TestRun (TestRunState 'PendingT))`.
- `planPending`: on `PendingLaunch`, if `descriptionKey tr `Set.member` launched`
  ⇒ `PendingAwaitObservation fact`; else `PendingLaunchOnly fact`. Other
  branches unchanged.

### Process.hs

- `launchPendingTest` returns `IO Bool` — `True` iff the launch POST
  (`pushTest`) succeeded.
- `executePendingAction :: ProcessOptions -> PendingAction -> IO (Maybe Text)`
  returns the description key to **keep marked** this poll:
  - `PendingLaunchOnly fact` → launch; `Just (descriptionKey tr)` iff success,
    else `Nothing` (failed launch ⇒ retried next poll — spec D2).
  - `PendingAwaitObservation fact` → log "awaiting observation"; `Just (descriptionKey tr)`.
  - `PendingAcceptObserved` / `PendingDrainDuplicate` / `PendingSkipUntrusted` → `Nothing`.
- Replace `forever $ runExceptT $ …` with manual recursion threading the
  marker set:
  ```haskell
  let loop launched = do
        e <- runExceptT (pollOnce opts launched)
        loop (either (const launched) id e)
  loop Set.empty
  ```
  `pollOnce :: ProcessOptions -> Set Text -> ExceptT () IO (Set Text)` runs the
  current body, collects the pending-action marker results, and returns
  `launched' = Set.fromList (catMaybes results)` (rebuilt each poll ⇒ bounded;
  spec FR3). `threadDelay` stays at the end of `pollOnce`. Running actions do
  not touch the marker. On a failed poll the old `launched` is kept.

### Tests (RED first)

- `PlanSpec`: add the `Set` argument (`mempty`) to every existing
  `planAgentPoll` call.
- new: `planAgentPoll trusted (Set.singleton matchingDescription) [] [pendingFact] []`
  ⇒ `pendingActions = [PendingAwaitObservation pendingFact]` (no second launch).
- new: empty marker set ⇒ `PendingLaunchOnly` (explicit "launch when not yet
  marked").

---

## Gate (both slices)

`./gate.sh` (bootstrapped in the worktree):
`git diff --check`, `just build`, `just unit`, `cabal-fmt -c`,
`fourmolu -m check src app test CI/rewrite-libs`, `just hlint`.
Plus `cabal check` (Hackage-ready) and the agent build
`nix build .#moog-agent` for the IO change in S2. Run locally before each
commit; never use CI to find lint/build errors.

## Commit shape

- S1: `fix(agent): drain duplicate Antithesis runs instead of stalling (#175)`
- S2: `fix(agent): make Antithesis launch idempotent via in-process marker (#175)`

Each body carries the `Tasks: T175-S<n>` trailer. One commit per slice,
bisect-safe, no push (orchestrator pushes after review).

---

## Slice 3 — prefer the authoritative terminal run as canonical (prod finding)

**Goal:** fix the mixed-outcome drain bug found in the live prod test — a
double-launched test with one `completed` and one `incomplete` run was finished
as `failure` because `canonicalRun = min run_id` picked the aborted run.

### State.hs
- Add `statusPriority :: AntithesisRunStatus -> Int` (or equivalent ranking),
  haddocked: `completed` highest (ran to completion → authoritative result),
  then `incomplete`, `cancelled`, then non-terminal (`in_progress`/`starting`),
  then `unknown` lowest.
- Change `canonicalRun` to pick the **highest-priority** run, tie-broken by
  ascending `run_id` for determinism:
  ```haskell
  canonicalRun =
      minimumBy (comparing (\r -> (negate (statusPriority (antithesisRunStatus r)), antithesisRunId r)))
  ```
  Still total only on non-empty input; still shared by both drains (FR6 holds).

### Tests (RED first)
- **Update** the existing "waits when canonical not terminal" cases (PlanSpec +
  StateSpec): they used `runA in_progress` + `runB completed`; with the ranking
  the canonical is now the **completed** `runB`, so the expectation flips from
  `RunningDrainWait runA` → finish from `runB` (success). Re-name them to state
  the new policy ("finishes from the completed run even when a duplicate is
  still running").
- **Add** the prod regression: `runA "run-a" incomplete+report`,
  `runB "run-b" completed+report` ⇒ canonical = `runB` (completed) ⇒
  `RunningFinishDuplicate runB OutcomeSuccess <report-url> [...]` — NOT failure
  from `run-a`. Mirror at the `runningDecision`/`planAgentPoll` level.
- **Keep** same-status cases (both completed / both incomplete) green: ranking
  ties → `run_id`, so canonical is unchanged for those.

### Gate
`nix run .#unit-tests` + `nix build .#moog-agent` (gate.sh was dropped at
finalize; commit `--no-verify`, log the deviation).

### Commit
`fix(agent): prefer the completed run as drain canonical (#175)` + `Tasks: T175-S3`.
