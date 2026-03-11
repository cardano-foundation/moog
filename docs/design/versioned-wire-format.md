# Versioned Wire Format for On-Chain Fact Types

## Problem

On-chain facts are immutable. When a type's serialization changes, the
system must read old facts forever. Currently this is handled by
asymmetric `FromJSON` instances that accept multiple formats but only
write the latest. This hides migration logic inside typeclass instances
and causes silent format rewriting during round-trips (e.g. when the
agent transitions a test-run state).

## Design

### Version tag on fact values

Every on-chain fact value carries a `"v"` field:

```json
{"v": 0, "phase": "pending", "duration": 3, ...}
{"v": 1, "phase": "pending", "duration": {"hours": 3}, ...}
```

- `ToJSON` always writes the **current** version.
- `FromJSON` reads the `"v"` field (defaulting to `0` if absent, for
  backwards compatibility with existing facts) and dispatches to the
  appropriate versioned decoder.
- Each version's decoder is **symmetric** — it knows exactly one format.

### Affected types

Fact values that go on-chain and have had (or may have) format changes:

| Type | Current version | v0 format |
|------|----------------|-----------|
| `TestRunState PendingT` | 1 | duration as `Int54` |
| `TestRunState RunningT` | 1 | wraps v0 PendingT |
| `TestRunState DoneT` | 1 | wraps v0 PendingT, duration as `Int54` |
| `Config` (via `TestRunValidationConfig`) | 1 | durations as `Int54`, no `protocolVersion` |

Types that are unaffected (no format changes):

- `RegisterUserKey` / `RegisterRoleKey` / `WhiteListKey` — key types
- `()` — trivial value for registrations/whitelist
- `TestRun` — key type, no Duration fields

### Implementation

Three helpers in `Core.Types.WireVersion`:

```haskell
currentWireVersion :: Int54        -- 1
readWireVersion    :: Map String JSValue -> m Int54
wireVersionField   :: (String, m JSValue)
```

- `wireVersionField` is added to every `ToJSON` object.
- `readWireVersion` is called in `FromJSON` to get the version
  (defaults to `0` when absent).

Version dispatch is inline in each `FromJSON` instance — no
typeclass needed. The only version-dependent logic is which
duration decoder to use:

```haskell
durationForVersion :: Int54 -> JSValue -> m Duration
durationForVersion 0 = durationFromV0  -- plain JSNum
durationForVersion _ = fromJSON        -- {"hours":n}
```

`Duration`'s `FromJSON` is now symmetric (object format only).
`durationFromV0` handles the legacy `JSNum` format and is called
by the v0 branches of `TestRunState` and `Config` decoders.

`Config` v0 also uses `testRunValidationConfigFromV0` to parse
nested `TestRunValidationConfig` with plain-number durations.

### What does NOT change

- Key types (`TestRun`, `ConfigKey`, etc.) — no versioning needed
- The fact envelope (`Fact k v`) — unchanged
- `parseFacts` — still uses `FromJSON`, which now dispatches via version
- The `ProtocolVersion` in `Config` — separate concern (client/oracle
  handshake), orthogonal to wire format versioning

### Modules changed

| Module | Change |
|--------|--------|
| `Core.Types.WireVersion` | New — version helpers |
| `Core.Types.Duration` | Symmetric `FromJSON`, new `durationFromV0` |
| `User.Types` | `ToJSON`/`FromJSON` for `TestRunState` write/read `"v"` |
| `Oracle.Config.Types` | `ToJSON`/`FromJSON` for `Config` write/read `"v"` |
| `Oracle.Validate.Requests.TestRun.Config` | New `testRunValidationConfigFromV0` |

### Test coverage

- Existing golden tests (v0 on-chain data) continue passing
- Existing round-trip tests verify v1 symmetry
