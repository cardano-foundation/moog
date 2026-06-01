# Spec — #147 moog builds & submits its token facts client-side

Parent epic: #146. Branch base: `moog-v2`. PR target: `moog-v2`.

## P1 user story

As a moog operator, when I run any token-mutating command, moog fetches
MPFS *facts* and builds, signs, and submits the transaction client-side,
reading token state from the new `/tokens/*` endpoints.

## Context

This is the foundation child of the moog-v2 cutover. It does not switch
any requester/oracle/agent flow yet; it lays the reusable mechanism the
flow children (#148–#150) build on, generalizing the boot/end facts
pattern already present in `MPFS.{API,Boot,End}`.

## Functional requirements

- FR1 — `cabal.project` pins `cardano-mpfs-offchain` (subdirs incl. any
  new `cage-tx` package) to the latest `main`
  (`c9eb9ea9acc6b928ec30d4fd6d88180811876147`) with an updated nix32
  `--sha256:`. The project builds against it.
- FR2 — (SPLIT to #155) moog reads token state via `GET /tokens/:id` (replacing legacy
  `token/:id`) and per-key facts via `GET /tokens/:id/facts/:key`.
- FR3 — client-side cage tx builders exist for request insert/delete/
  update, update, and retract, each producing an unsigned tx from
  server-returned facts (mirroring `bootTokenFromFacts`/`endTokenFromFacts`).
- FR4 — the `MPFS m` record exposes these facts-based operations; legacy
  operations remain (deleted later in #151).

## Success criteria

- Unit tests prove facts → unsigned tx for each new builder, and decode
  of `/tokens/:id` `TokenResponse`.
- `./gate.sh` green at HEAD (`nix build .#moog-agent .#unit-tests`).
- No requester/oracle/agent flow behavior changed; boot/end pattern
  generalized, not duplicated.

## Non-goals

- Switching role flows (#148–#150) or deleting legacy routes (#151).
- `/facts/reject`, `/tx/sweep` (out of epic scope).

## Inherited invariants (from #146)

- Facts-only, client-side tx construction; external-signing model.
- Do not reshape concerns owned elsewhere; PR targets `moog-v2`.
