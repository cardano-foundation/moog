# Implementation Plan: Antithesis Runs CLI

## Modules

- `src/User/Antithesis/Constants.hs` embeds the public OAuth client id and
  scope constants.
- `src/User/Antithesis/Login.hs` owns the GitHub OAuth cache file and the
  `ensureToken` boundary.
- `src/User/Antithesis/ProxyClient.hs` owns the HTTP call to the proxy,
  response classification, JSON decoding, and one-time reauth retry.
- `src/User/Antithesis/Cli.hs` owns the optparse command group and command
  execution.
- `src/Cli.hs` and `src/Options.hs` wire the new command into the existing
  executable.

## Slices

### Slice 1: Parser and command wiring

Add the Antithesis command type, parser, and top-level `Cli.Command`
constructor. Prove with a parser unit test that `antithesis runs` resolves to
the new command. This slice may include a stub runtime that exits only after
later slices fill the behavior.

### Slice 2: Login token cache

Implement constants plus token cache read/write/delete helpers and
`ensureToken`. Prove cache miss writes the token, cache hit avoids the device
flow callback, and the token file mode is `0600`.

### Slice 3: Proxy client and runtime command

Implement proxy URL resolution, `GET /api/v1/runs`, JSON decoding, error
classification, 401 cache invalidation and one retry, command-level stderr and
exit codes, and `ExitCode` passthrough in `App`. Prove the 401 evict/retry path
with a local WAI proxy test.

## Verification

Each slice runs its focused RED test to failure first, then GREEN, then
`./gate.sh`. Final verification repeats `./gate.sh` at branch head and records
manual smoke status.
