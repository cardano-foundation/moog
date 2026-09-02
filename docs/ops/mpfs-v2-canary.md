# MPFS v2 Boot/End Boundary Canary

The canary proves the MPFS v2 live boundary needed to boot a cage token
and then end that same token through facts-only client construction.

It runs this path:

```text
wallet -> GET /status -> POST /facts/boot -> verifyBootFacts -> bootCageTx -> sign -> POST /tx/submit -> GET /tx/:id -> GET /tokens/:id -> GET /status -> POST /facts/end -> verifyEndFacts -> endCageTx -> sign -> POST /tx/submit -> GET /tx/:id -> GET /tokens/:id until 404
```

Required environment:

```bash
export MPFS_DEVNET_SERVER=/path/to/mpfs-devnet-server
export MPFS_BLUEPRINT=/path/to/mpfs/blueprint.json
export E2E_GENESIS_DIR=/path/to/devnet/genesis
export MPFS_DEVNET_PATH=/path/containing/cardano-node
export MOOG_CANARY_POLLS=240
```

Run:

```bash
nix run .#moog-mpfs-v2-canary
```

Successful output contains:

```json
{
  "boundary": "mpfs-v2-end",
  "bootTransactionObserved": true,
  "endTransactionObserved": true,
  "tokenObservedBeforeEnd": true,
  "tokenGoneObserved": true
}
```

This command is release evidence only for the MPFS v2 boot/end boundary.
It does not validate requester, oracle, agent, or Antithesis test-run
semantics.

## Dependency pins: what moves together (C-DEP-PINS)

Moog builds cage transactions against the `cardano-mpfs-onchain` types
directly, so it declares its own `cardano-mpfs-onchain`
source-repository-package in `cabal.project`. In the composed build moog's
declaration wins over the one in the pinned `cardano-mpfs-offchain`
revision. When the two disagree, the build fails with an unrelated-looking
type error (a missing record field such as `stateStakeScript`) instead of
naming the version skew that caused it.

`just dep-pins` compares the two declared revisions, reading both from
their real sources — moog's from `cabal.project`, the offchain one from
that pinned revision's own `cabal.project` on the public raw GitHub
endpoint (no credential). A passing run prints one canonical line carrying
the provenance of every input it compared:

```text
dep-pins: OK moog_onchain=<40hex> offchain_rev=<40hex> offchain_onchain=<40hex> project=cabal.project source=<https url>
```

It exits non-zero naming both revisions and both file positions when they
differ, and fails closed when a source cannot be read. CI runs it on pull
requests before the unit-test build.

The check honours exactly two environment inputs, and both are overrides:

- `DEP_PINS_OFFCHAIN_REV` replaces the offchain revision read from
  `cabal.project`;
- `DEP_PINS_PROJECT_FILE` replaces the project file read as moog's side.

While either override is set, the canonical `dep-pins: OK` token is never
emitted: an agreeing run prints `dep-pins: PASS-OVERRIDE …` naming the
override instead, so a green about a redirected source can never be
mistaken for a green about moog's declared sources. A mismatch fails as it
always does.

When the offchain pin moves, the `cardano-mpfs-onchain` pin must move with
it, to the revision that new offchain revision declares, and **both**
`--sha256` comments must be refreshed. To preview a hypothetical offchain
revision before pinning it:

```bash
DEP_PINS_OFFCHAIN_REV=<40-hex revision> just dep-pins
```

The CI smoke for this boundary runs the dedicated Haskell executable
`moog-mpfs-v2-canary`. It expects the paired offchain devnet executable
and its Nix-provided runtime paths:

```bash
export MPFS_DEVNET_SERVER=/path/to/mpfs-devnet-server
export MPFS_BLUEPRINT=/path/to/mpfs/blueprint.json
export E2E_GENESIS_DIR=/path/to/devnet/genesis
export MPFS_DEVNET_PATH=/path/containing/cardano-node
nix run .#moog-mpfs-v2-canary
```

The executable creates a deterministic MOOG wallet, patches a temporary
copy of the devnet genesis to fund that wallet, starts
`mpfs-devnet-server`, and runs the boot-then-end canary path against the
live HTTP API.

## Full v2 facts flow demo

This cast shows the facts-only lifecycle against a local MPFS devnet:
wallet creation and genesis funding, fast token boot, a config request
applied by the oracle, an expired request rejected by the oracle, and a
fresh request rejected too early by the Phase-3 guard.

```asciinema-player
{ "file": "assets/video/v2-full-flow.cast"
, "mkap_theme": "none"
, "cols": 80
}
```
