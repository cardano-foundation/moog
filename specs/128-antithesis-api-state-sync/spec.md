# Feature Specification: Antithesis API State Synchronization

## Goal

`moog-agent` must read Antithesis off-chain lifecycle state from the
Antithesis API before it launches or writes any on-chain test-run state
transition. The agent must not treat launch POST success as accepted
state, must not repost a pending test when a matching Antithesis run is
already visible, and must only publish `accepted` / `finished` on-chain
states that are backed by an API observation.

## P1 user story

As a `MOOG operator`, I `run moog-agent while Antithesis runs are pending, accepted, and finished across chain/off-chain boundaries` and observe `the agent launches at most one off-chain run per on-chain test and mirrors on-chain state only after reading the corresponding Antithesis API state`.

## Functional requirements

- **FR-001**: The agent has a typed Antithesis run observation layer for
  `GET /api/v0/runs`, including `run_id`, lifecycle `status`,
  `parameters.antithesis.description`, and `links.triage_report`.
- **FR-002**: The agent authenticates read-API requests with a
  non-interactive Antithesis API key. The GitHub-device-flow proxy client
  remains a human/CLI surface, not the daemon path.
- **FR-003**: Before launching a pending on-chain test, the agent queries
  API-visible runs and matches them to the on-chain `TestRun` by the
  deterministic `antithesis.description` payload already sent in POST.
- **FR-004**: If exactly one matching API run exists for a pending
  on-chain test, the agent does not POST again. It may submit
  `pending -> accepted` only from that API observation.
- **FR-005**: If no matching API run exists for a trusted pending test,
  the agent may POST the launch but must not submit `pending -> accepted`
  in the same polling round.
- **FR-006**: If more than one matching API run exists, the agent fails
  closed for that test: no POST, no on-chain transition, and a clear
  duplicate-run log line naming the run ids.
- **FR-007**: For a running on-chain test, the agent may submit
  `accepted -> finished` only when exactly one matching API run is in a
  terminal status with a triage report URL.
- **FR-008**: Terminal API status maps to on-chain outcome as follows:
  `completed` -> `success`; `cancelled` and `incomplete` -> `failure`;
  `unknown`, `starting`, and `in_progress` do not produce `finished`.
- **FR-009**: The legacy email parser remains available in this ticket;
  removal belongs to #129. The service loop must not use email results
  for the #128 synchronization path.

## Success criteria

- A pending on-chain test with a matching API run is accepted on-chain
  without another POST.
- A pending on-chain test with no matching API run is POSTed at most once
  per poll and left pending until a later API observation.
- A running on-chain test with a completed API run and report URL is
  finished on-chain from API data.
- Missing or duplicate API matches never mutate on-chain state.
- The package-level gate in `gate.sh` exits 0.

## Non-goals

- Removing `User.Agent.PublishResults.Email` or `collect-results-*`
  commands.
- Adding SSE/event-stream support.
- Cancelling or deleting duplicate Antithesis runs. The documented API
  exposes read/list/detail/log/property endpoints, not a supported stop
  or cleanup operation.
- Treating local push or POST success as proof of Antithesis accepted
  state.
