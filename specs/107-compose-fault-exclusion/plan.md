# Plan — moog#107

## Tech stack

- Haskell, GHC pinned by `cabal.project` (existing project pin).
- `yaml` (already in moog's `library` build-depends — used elsewhere for compose-reading; verified at `moog.cabal:229`).
- `aeson` for the JSON serializer change (already a dep).
- `hspec` + `QuickCheck` for tests, alongside the existing unit-tests test-suite at `test/`.

No new third-party dependencies are required.

## Module layout

```text
src/Docker/FaultExclusion.hs            # NEW. Parses compose, classifies services.
src/User/Agent/PushTest.hs              # extended: 4 new fields + ToJSON update + wiring.

test/Docker/FaultExclusionSpec.hs       # NEW. Parser tests.
test/User/Agent/PushTestSpec.hs         # extended: ToJSON tests for the new keys.
```

`Docker.FaultExclusion` lives next to the existing `Docker` module. Public surface:

```haskell
module Docker.FaultExclusion
    ( FaultClass(..)            -- Network | Kill | Pause | Stop
    , FaultExclusions(..)       -- four [Text] lists, ordered by compose appearance
    , parseFaultExclusions      -- FilePath -> IO (Either String FaultExclusions)
    , emptyFaultExclusions      -- FaultExclusions{ all four lists empty }
    ) where
```

The parser is `IO` only because it reads the compose file. The pure core (`Yaml.Value -> Either String FaultExclusions`) sits below it so tests don't need the FS.

`PostTestRunRequest` grows four fields:

```haskell
data PostTestRunRequest = PostTestRunRequest
    { -- existing fields...
    , networkFaultExclusion            :: [String]
    , containerFaultsKillExclusion     :: [String]
    , containerFaultsPauseExclusion    :: [String]
    , containerFaultsStopExclusion     :: [String]
    }
```

`ToJSON` emits each `custom.*_exclusion` key only when its list is non-empty (`mconcat` of conditional `[Pair]` chunks).

## Slices

Each slice is one bisect-safe commit. Slice boundaries are vertical (parser → serializer → wiring) so reviewers can read them as the design steps they actually are.

### Slice 1 — `T107-S1` — Compose-label parser

Add `Docker.FaultExclusion` + tests. No callers yet; module is exposed in `moog.cabal` so the unit suite builds against it. Strictly additive.

RED: `test/Docker/FaultExclusionSpec.hs` with the eight cases listed in tasks.md, all failing.
GREEN: minimal parser implementing F1–F4.
Bisect-safe because: no caller touched, so the module compiling and its tests passing is the whole contract.

### Slice 2 — `T107-S2` — `PostTestRunRequest` fields + `ToJSON`

Extend `User.Agent.PushTest.PostTestRunRequest` with the four `[String]` fields. Update `ToJSON`. Extend the existing `User.Agent.PushTestSpec` with the three serialization cases. **Default the four fields to `[]` at all call sites** so the rest of the codebase still builds — the wiring slice replaces the defaults with parser output.

RED: new cases in `PushTestSpec` fail because the keys aren't there yet.
GREEN: add the fields, update the serializer, fix call sites with `[]` placeholders.
Bisect-safe because: with all four lists `[]`, F6 says the keys are omitted — payload is byte-identical to today's.

### Slice 3 — `T107-S3` — Wire parser into `pushTestToAntithesisIO`

In `pushTestToAntithesisIO`, call `parseFaultExclusions (context </> "docker-compose.yaml")`, populate the four fields on `PostTestRunRequest` from the result. Replace the `[]` defaults from Slice 2.

RED: a new integration test in `PushTestSpec` (or a small fixture-based test) exercises the path: a temp dir with a stub compose containing one service labeled across all four classes → request carries that service in all four `custom.*_exclusion` fields.
GREEN: wire it.
Bisect-safe because: a compose without `com.antithesis.exclude_from_faults` labels yields `emptyFaultExclusions`, F6 still suppresses the keys, and existing call sites pass through unchanged. The change is purely an upgrade from "always empty" to "compose-derived" lists.

## Risks + mitigations

- **Compose parsing surprise (anchors, includes).** The yaml library handles anchors. compose files in `cardano-node-antithesis` don't use `extends:` or compose-file `include:`. Parser stays at YAML-level, doesn't try to resolve compose semantics.
- **Label value with single quotes vs unquoted comma.** YAML's flow scalar handling treats `"network,kill"` and `network,kill` identically once parsed to `Text`. Tests cover both.
- **Service without any labels.** The label map is absent → the service contributes to no exclusion class. No special case needed.
- **Compose file missing entirely.** Parser returns `Left "<path>: no such file"`. Wiring slice decides whether to treat this as fatal or `emptyFaultExclusions`. Decision: **fatal** — silent fallback would mask config rot.

## Gate

`./gate.sh` (in worktree root) runs `nix develop -c cabal build all --enable-tests`, `cabal test unit-tests`, `cabal-fmt -c moog.cabal`, `fourmolu -m check`, `hlint -c`. Each slice must leave it green.
