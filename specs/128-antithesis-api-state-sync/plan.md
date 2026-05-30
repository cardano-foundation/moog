# Implementation Plan: Antithesis API State Synchronization

## Technical context

The current agent loop does two unsafe things:

- it polls/parses completion email and publishes `finished` from those
  parsed emails;
- it posts a launch and immediately submits `pending -> accepted`, even
  though the launch endpoint does not return an Antithesis run id or an
  authoritative accepted state.

Main already has the human `moog antithesis ...` proxy client, but that
path needs an interactive GitHub OAuth token. The daemon needs a direct
Antithesis read-API config using `MOOG_ANTITHESIS_API_KEY` /
`antithesisApiKey` and `MOOG_ANTITHESIS_API_URL` /
`antithesisApiUrl` (default derived from the launch URL when possible).

## Slices

### Slice 1 — Typed API observation and reconciliation core

Add `User.Agent.Antithesis.State` with:

- `AntithesisRunStatus`
- `AntithesisRun`
- JSON parsing for `GET /api/v0/runs`
- deterministic matching by `parameters.antithesis.description`
- pure pending/running reconciliation decisions

Tests cover parsing, duplicate detection, no-repost pending decisions,
terminal outcome mapping, and fail-closed ambiguous states.

### Slice 2 — Agent configuration and direct read API client

Add `User.Agent.Antithesis.Client` with non-interactive Bearer API-key
requests to `/api/v0/runs`, including pagination through `next_cursor`.
Thread `AntithesisApiConfig` through `ProcessOptions`.

Tests cover config parsing from YAML/env and URL derivation from the
launch URL.

### Slice 3 — Service loop synchronization

Replace the service loop's email-result path with API reconciliation:

- fetch runs once per poll;
- for pending tests, reconcile before launch; launch only on no match and
  do not submit accepted in that same round;
- for running tests, finish only from a single terminal API observation
  carrying a report URL;
- log missing/duplicate/in-progress states.

Tests cover the pure per-poll action plan and avoid live Antithesis.

## Gate

`./gate.sh` runs:

```bash
git diff --check
nix build --quiet --no-link .#moog-agent .#unit-tests
```

The full `nix develop`/`just` gate is red on current `main` before #128
work because the dev shell tries to build `cabal-fmt-0.1.12` with
`base-4.21.1.0`. This PR keeps that baseline blocker visible in `WIP.md`
and uses the package-level Nix checks that are green on the fresh
worktree.
