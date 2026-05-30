# Tasks: Antithesis API State Synchronization

## Slice 0 — Bootstrap

- [X] T000 Create issue worktree, `gate.sh`, and draft PR.

## Slice 1 — Typed API observation and reconciliation core

- [X] T128-S1 Add RED unit tests for Antithesis run parsing and matching.
- [X] T128-S1 Add RED unit tests for pending/running reconciliation decisions.
- [X] T128-S1 Implement the typed observation/reconciliation module.
- [X] T128-S1 Run the focused unit tests and `./gate.sh`.
- [X] T128-S1 Commit the accepted slice with `Tasks: T128-S1`.

## Slice 2 — Agent configuration and direct read API client

- [X] T128-S2 Add RED tests for API config parsing and launch-URL base derivation.
- [X] T128-S2 Add the direct Bearer API-key run-list client with pagination.
- [X] T128-S2 Thread API config through `ProcessOptions`.
- [X] T128-S2 Run the focused unit tests and `./gate.sh`.
- [X] T128-S2 Commit the accepted slice with `Tasks: T128-S2`.

## Slice 3 — Service loop synchronization

- [X] T128-S3 Add RED tests for one-poll agent actions: no duplicate POST, delayed accepted, finished from API, duplicate fail-closed.
- [X] T128-S3 Replace email-result service-loop decisions with API-backed reconciliation.
- [X] T128-S3 Keep legacy email commands untouched for #129.
- [X] T128-S3 Run the focused unit tests and `./gate.sh`.
- [X] T128-S3 Commit the accepted slice with `Tasks: T128-S3`.
