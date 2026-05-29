# Tasks: Antithesis Runs CLI

## Slice 1: Parser and command wiring

- [X] T114-S1 Add `User.Antithesis.Cli` command type and parser for
  `antithesis runs`.
- [X] T114-S1 Wire the command group into `Options.commandParser` and
  `Cli.Command`.
- [X] T114-S1 Add a parser unit test that observes the new command.
- [X] T114-S1 Run the focused RED/GREEN test and `./gate.sh`.

## Slice 2: Login token cache

- [ ] T114-S2 Add OAuth constants and token cache helpers.
- [ ] T114-S2 Implement `ensureToken` with device-flow fallback and stderr
  prompt callback.
- [ ] T114-S2 Add unit tests for cache write, cache read, and mode `0600`.
- [ ] T114-S2 Run the focused RED/GREEN test and `./gate.sh`.

## Slice 3: Proxy client and runtime command

- [ ] T114-S3 Implement proxy URL resolution, runs HTTP request, JSON decode,
  and error classification.
- [ ] T114-S3 Implement 401 cache eviction and one reauth retry.
- [ ] T114-S3 Map runtime outcomes to stdout/stderr and exit codes 0/2/3/4.
- [ ] T114-S3 Add unit coverage for cache eviction on 401 and retry.
- [ ] T114-S3 Run focused RED/GREEN tests, `./gate.sh`, and record smoke
  status.
