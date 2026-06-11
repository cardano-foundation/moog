# Plan — #190 property-derived outcome

Base: built on the #175 fix (df8d8ab1). The S4 commit (c1012bf5) added the
mechanism: `RunProperty`/`PropertiesPage` parsing, `listAllProperties`
(paginated), the `/properties` `after` query param, and `resolveFinishOutcome`
in Process.hs (a `completed`+`success` finish fetches properties and downgrades
to `failure` when `runFailedAssertions`). This plan refines the **predicate**.

## Refinement — catch everything except a platform exclusion list

Operator policy (2026-06-11): a run is `success` only if green on **both** the
platform and the SUT/moog checks; do NOT mask. So **any** failing property
(event OR assert) counts as failure, EXCEPT a maintained list of
platform/Antithesis properties we cannot control (escalated upstream).

Two defects in the shipped `runFailedAssertions = any (failing && not event)`:
1. it counts `Unique Edges` (a universal Antithesis-platform assert) ⇒ marks
   ~every run failure;
2. it ignores `is_event=true` ⇒ MISSES real SUT failures that are event-typed
   (`asteria-game/*.sh`, `container: …, exit code: 1`, `cluster fork …`).

### State.hs
- `platformExcludedProperties :: [Text]` (exported, haddocked as the
  upstream-escalation list; refine as Antithesis fixes them):
  `"Unique Edges"`, `"Sometimes: Root moments"`, `"node - throttle"`,
  `"No Antithesis errors"`, `"systemd-journal"`,
  `"Never: There are events with very high output that will fail to materialize & artifact from."`
- Rewrite `runFailedAssertions :: [RunProperty] -> Bool` to:
  `any (\p -> propFailing p && propName p `notElem` platformExcludedProperties)`
  — i.e. catch every failing property (event or assert) whose name is not
  excluded. (Drop the `is_event` restriction.)
- Keep `RunProperty`/`PropertiesPage`/parsing as-is.

### StateSpec
- A failing **event** that is a SUT check (e.g. `asteria-game/eventually_alive.sh`)
  ⇒ `runFailedAssertions == True` (regression: events now count).
- A failing **excluded** property (`Unique Edges`, `Sometimes: Root moments`)
  ⇒ does NOT by itself make `True`.
- All-passing or only-excluded-failing ⇒ `False`.
- A failing non-excluded assert (`Unexpected terminations: state_timed_out`)
  ⇒ `True` (unchanged).

### Gate / commit
`nix run .#unit-tests` + `nix build .#moog-agent`. One commit
`fix(agent): catch all non-platform failing properties; exclude platform list (#190)`
+ `Tasks: T190-S2`.
