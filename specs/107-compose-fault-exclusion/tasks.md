# Tasks — moog#107

## Slice 1 — Compose-label parser

- [ ] T107-S1.1 Add `Docker.FaultExclusion` to `moog.cabal` `library` `exposed-modules` and add the corresponding spec module to `test-suite unit-tests` `other-modules`.
- [ ] T107-S1.2 RED: write `test/Docker/FaultExclusionSpec.hs` with these cases — empty compose / no labels → all four lists empty; one service labeled `"network"` → only network list contains it; one service labeled `"network,kill,pause,stop"` → all four lists contain it; two services labeled disjoint classes → each class list contains its service; whitespace-padded value `" network , kill "` → parsed identically to `"network,kill"`; empty token between commas (`"network,,kill"`) → parsed as `["network","kill"]`; unknown token `"netwrk"` → `Left` naming the service + the offending token; missing compose file → `Left` with path. Tests fail because module + types don't exist yet.
- [ ] T107-S1.3 GREEN: implement `Docker.FaultExclusion` (`FaultClass`, `FaultExclusions`, `parseFaultExclusions`, pure helper `classifyServices :: Yaml.Value -> Either String FaultExclusions`, `emptyFaultExclusions`).
- [ ] T107-S1.4 Run `./gate.sh` green. WIP.md milestone: `Slice 1 gate green`.
- [ ] T107-S1.5 Commit (signed) with subject `feat(agent): compose-label-driven fault-exclusion parser` and trailer `Tasks: T107-S1`.

## Slice 2 — PostTestRunRequest fields + JSON serializer

- [ ] T107-S2.1 RED: extend `test/User/Agent/PushTestSpec.hs` with three cases — `PostTestRunRequest` with all four exclusion lists empty serializes to a payload with **no** `custom.*_exclusion` keys; populated list emits `"custom.container_faults_kill_exclusion": "asteria-game,tx-generator"` (comma-joined, no spaces); mixed empty + populated emits only the populated keys. Tests fail because fields and serializer logic don't exist yet.
- [ ] T107-S2.2 GREEN: add `networkFaultExclusion`, `containerFaultsKillExclusion`, `containerFaultsPauseExclusion`, `containerFaultsStopExclusion` to `PostTestRunRequest`. Update `ToJSON` to emit each `custom.*_exclusion` key conditionally (omit on empty). Update every existing call site of `PostTestRunRequest` constructor to default the four fields to `[]` so the codebase still builds.
- [ ] T107-S2.3 Run `./gate.sh` green. WIP.md milestone: `Slice 2 gate green` + note the existing `PushTestSpec` cases still pass (proves F8 backwards compatibility).
- [ ] T107-S2.4 Commit (signed) with subject `feat(agent): emit custom.*_exclusion params in launch payload` and trailer `Tasks: T107-S2`.

## Slice 3 — Wire parser into pushTestToAntithesisIO

- [ ] T107-S3.1 RED: integration test in `PushTestSpec` — set up a `withSystemTempDirectory` containing a stub `docker-compose.yaml` with one labeled service, run the relevant slice of `pushTestToAntithesisIO`'s logic (the part that builds `PostTestRunRequest` from `Directory context`; factor out a pure helper if needed to make it testable without doing the HTTP POST), assert the request carries the service in all four exclusion fields.
- [ ] T107-S3.2 GREEN: in `pushTestToAntithesisIO`, call `parseFaultExclusions ((unDirectory context) </> "docker-compose.yaml")`. On `Left`, throw `PushFailure` (new constructor `ComposeFaultExclusionParseFailure String`). On `Right`, populate the four fields on `PostTestRunRequest`. Remove the `[]` placeholders from the call site introduced in Slice 2.
- [ ] T107-S3.3 Run `./gate.sh` green. WIP.md milestone: `Slice 3 gate green` + a note recording the integration test's tmpdir compose content for posterity.
- [ ] T107-S3.4 Commit (signed) with subject `feat(agent): wire compose fault-exclusion parser into push payload` and trailer `Tasks: T107-S3`.

## Finalization (orchestrator)

- [ ] T107-F1 PR body audit — confirm the three slice subjects appear in the merge log and the test plan checkboxes match what shipped.
- [ ] T107-F2 Drop `gate.sh` in a `chore(#107): drop gate.sh (ready for review)` commit (signed).
- [ ] T107-F3 `gh pr ready 108` to flip out of draft.
- [ ] T107-F4 Wait for review + merge. Do not self-merge.

## Post-merge cleanup (orchestrator)

- [ ] T107-C1 Tag a moog release that includes this PR so cardano-node-antithesis CI can bump to it.
- [ ] T107-C2 Open the follow-up PR on `cardano-foundation/cardano-node-antithesis` adding `com.antithesis.exclude_from_faults` labels to `tracer`, `tracer-sidecar`, `sidecar`, `tx-generator`, `asteria-game` (master) and the same plus `adversary` (adversary testnet), and bumping the moog release tag in `.github/workflows/cardano-node.yaml`.
- [ ] T107-C3 After 2-3 consecutive trys on the master testnet show zero new findings on the three Asteria scripts, open the cleanup PR removing `sdk_install_signal_trap` from `components/asteria-game/composer/asteria-game/*.sh`.
- [ ] T107-C4 `git worktree remove /code/moog-issue-107`, `git branch -D feat/compose-fault-exclusion`, `git push origin --delete feat/compose-fault-exclusion`, `git worktree prune`.
