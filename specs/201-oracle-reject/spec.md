# Spec — #201 oracle reject for expired/dishonest requests

Parent epic: #181. Branch base: `moog-v2`. PR target: `moog-v2`.

## P1 User Story

As an oracle operator, I run a moog reject command and observe expired or
dishonest pending requests cleared from the token, with request owners refunded
by the on-chain Phase-3 path.

## Context

The oracle update path already validates chosen pending requests and submits a
facts-built cage transaction through `MPFS.Update` and `MPFS.API`. The MPFS
offchain server already exposes `POST /facts/reject`, but moog does not surface
that endpoint in its MPFS record or in `oracle token`.

`Oracle.Types` already includes `RejectRequest` for a test-run rejection fact.
That is separate from the MPFS Phase-3 reject operation in this ticket: the new
command rejects pending request UTxOs at the cage layer instead of creating an
agent-level rejected test-run fact.

## Functional Requirements

- FR1 — `MPFS.API` exposes `POST /facts/reject`, verifies returned reject facts
  against the current trusted root, and builds the unsigned cage reject
  transaction with the offchain reject builder.
- FR2 — the `MPFS m` record has an `mpfsRejectTokenFromFacts` operation that
  mirrors `mpfsUpdateTokenFromFacts` and accepts a token plus an explicit
  non-empty request-ref subset.
- FR3 — moog exposes `oracle token reject -t/--token-id ... -w/--wallet ... -o
  <request-ref>...` and submits through `mpfsRejectTokenFromFacts`.
- FR4 — the reject command is oracle-only: the signing wallet owner must match
  the token owner, the token must parse, the token must have pending requests,
  every requested ref must exist on the token, and an empty subset fails before
  submission.
- FR5 — wrong subsets fail closed. A processable request or otherwise
  non-rejectable request must not be silently included; MPFS `/facts/reject` and
  `verifyRejectFacts` are the source of truth for Phase-3 eligibility.
- FR6 — the MPF root is unchanged by a successful reject; rejected request UTxOs
  are consumed and owner refunds are produced according to the on-chain
  conservation rule.

## Success Criteria

- Unit tests prove reject request-body construction, MPFS record wiring, and
  `oracle token reject` routing/validation behavior.
- The focused gate passes for each slice and the final `./gate.sh` passes at
  HEAD.
- Integration or live-boundary evidence demonstrates an expired request can be
  rejected against MPFS and that a non-expired/processable subset is refused.

## Non-Goals

- Requester-side retract; `moog retract` already covers Phase 2.
- Changing validator economics, refund math, tip policy, or token boot
  parameters.
- Changing the agent `reject-test` command or test-run rejection fact semantics.

