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

### Implementation approach

A `Versioned` wrapper type that handles the dispatch:

```haskell
-- The version tag
newtype WireVersion = WireVersion Int

-- Typeclass for versioned on-chain types
class VersionedJSON a where
    currentWireVersion :: WireVersion
    encodeVersioned :: a -> JSValue  -- always current version
    decodeVersioned :: WireVersion -> JSValue -> Maybe a

-- ToJSON writes current version + payload
-- FromJSON reads "v" field, dispatches to decodeVersioned
```

The `FromJSON Duration` becomes symmetric (object format only).
The v0 migration logic moves into `decodeVersioned` for `TestRunState`
and `Config`, where it converts `JSNum` durations to `Duration` values
before constructing the Haskell type.

### What does NOT change

- Key types (`TestRun`, `ConfigKey`, etc.) — no versioning needed
- The fact envelope (`Fact k v`) — unchanged
- `parseFacts` — still uses `FromJSON`, which now dispatches via version
- The `ProtocolVersion` in `Config` — separate concern (client/oracle
  handshake), orthogonal to wire format versioning

### Migration path

1. Add `"v"` field support to `TestRunState` and `Config` codecs
2. Make `Duration` codecs symmetric (object format only)
3. Move the `directHours` fallback into versioned decoders for v0
4. Existing golden tests continue passing (old data parses as v0)
5. New round-trip tests verify symmetry at each version
