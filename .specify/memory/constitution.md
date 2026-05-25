<!--
Sync Impact Report
Version change: 0.1.0 -> 0.2.0
Modified principles:
- Workflow Contracts Are Binding -> Workflow Governance And Reviewable History
- Specification Precedes Implementation -> Specification Precedes Implementation
- Test-First Slices And Evidence-Based Verification -> Verification Mirrors Risk
- Explicit, File-Backed Operations -> External Boundaries Are Explicit Effects
- Haskell And Nix Quality Is The Baseline -> Haskell, Nix, And CLI Quality
Added principles:
- Cardano-Certified Antithesis Coordination
- Role-Separated Protocol Authority
- MPFS Fact Integrity And Fail-Closed Validation
- Security And Operational Safety
- Documentation And Deployment Are Product Surface
Added sections:
- Purpose And System Model
Removed sections: none
Templates requiring updates:
- not present: .specify/templates/plan-template.md
- not present: .specify/templates/spec-template.md
- not present: .specify/templates/tasks-template.md
- not present: .specify/templates/commands/*.md
Deferred items: none
-->

# Moog Constitution

## Purpose And System Model

Moog exists to make Antithesis testing available to Cardano ecosystem
developers while recording the resulting identities, permissions, test-run
requests, state transitions, and result references in Cardano-certified MPFS
state. The repository is therefore both an operator-facing Haskell CLI and a
protocol participant implementation for three roles: requester, agent, and
oracle.

The system boundary is broader than the `moog` executable. It includes the
long-running `moog-agent` and `moog-oracle` services, MPFS REST interaction,
Cardano transaction signing and submission, GitHub identity and repository
checks, Docker Compose validation of submitted test assets, Antithesis API and
registry access, email result ingestion, Nix/Cabal packaging, Docker images,
deployment Compose files, and operator/user documentation. Changes MUST treat
those surfaces as one product unless a specification explicitly narrows scope.

The repository's default platform assumptions are GitHub for identity,
CODEOWNERS for repository role authority, MPFS for facts and transactions,
Cardano preprod for current chain interaction, and Antithesis for execution.
Any new platform, network, tenant, registry, or state backend MUST be explicit
in configuration, documentation, validation, and tests.

## Core Principles

### I. Cardano-Certified Antithesis Coordination

Moog MUST preserve its central purpose: coordinating Antithesis test execution
for Cardano components while making the coordination auditable through MPFS
facts certified on Cardano. User registration, repository role registration,
repository whitelisting, test creation, agent acceptance or rejection, and
result publication are protocol events, not incidental application state.

Every behavior-changing feature MUST state which protocol event, role, and
state transition it affects. Changes that only improve packaging, docs, or
operator ergonomics MUST still avoid weakening the traceability of test-run
state and result references.

Rationale: Moog is valuable because it connects scarce Antithesis access with
transparent Cardano ecosystem governance. Losing the on-chain trace or making
execution state ambiguous undermines the repository's reason to exist.

### II. Role-Separated Protocol Authority

Requester, agent, and oracle responsibilities MUST remain separate.
Requesters create user, role, and test-run requests. Agents whitelist or
blacklist repositories, validate and download assets, push tests to
Antithesis, accept or reject runs, and report encrypted result references.
Oracles validate pending requests against GitHub, signatures, protocol
configuration, and current MPFS state before committing state updates.

Code MUST NOT move oracle authority into requester or agent paths, bypass
agent whitelisting for real test execution, or let a convenience command submit
state transitions that the protocol requires another role to validate. Shared
helpers are allowed only when their call sites preserve role-specific
authorization and failure behavior.

Rationale: The protocol relies on different actors having different powers.
Role shortcuts create invisible trust changes and can turn a CLI convenience
into a governance bypass.

### III. MPFS Fact Integrity And Fail-Closed Validation

MPFS facts, changes, operations, token requests, and request-zoo constructors
are the canonical representation of Moog state. Serialization MUST remain
canonical JSON where the existing protocol uses canonical JSON, fact keys MUST
remain hash-addressable, and parsers MUST fail closed for malformed, unknown,
or unsupported requests.

Validation MUST remain centralized around explicit request validators and MUST
keep configuration absence, protocol-version mismatch, duplicate pending
requests, missing GitHub evidence, invalid signatures, and illegal state
transitions as first-class failures. Unknown insert, update, or delete requests
MUST NOT be treated as successful no-ops.

Rationale: MPFS gives Moog a verifiable state root only if the application
keeps its own fact schema, parser, and validator semantics precise. Silent
acceptance or ad hoc JSON compatibility creates false state history.

### IV. External Boundaries Are Explicit Effects

GitHub, MPFS, Antithesis, Docker, email, filesystem, wallet, SSH key, and
network interactions MUST remain behind explicit parser, context, effect, or
service boundaries. Business validation SHOULD be testable with mock effects
where practical; live-boundary behavior MUST have either an integration, E2E,
canary, smoke test, or documented operator verification path proportional to
the risk.

Configuration MUST be explicit and fail closed when required MPFS host, token,
wallet, tenant, registry, secret, or Docker setting is missing. Shipped
operator paths MUST NOT depend on hidden tenant-specific source defaults.
Secrets and runtime settings SHOULD be file-backed through stable mounted
paths when that makes rotation and deployment safer.

Rationale: Moog spends most of its time at system boundaries. The repository's
effect model is how pure protocol decisions stay reviewable while operators
can still run real services.

### V. Security And Operational Safety

Wallet files, wallet passphrases, GitHub PATs, Antithesis credentials, Docker
registry credentials, Slack webhooks, and result URLs MUST be treated as
operator secrets. Documentation and deployment files MUST preserve restricted
secret mounts, minimum wallet balance guidance, PAT rotation guidance, and
clear separation between oracle and agent credentials.

Agent Docker privileges and Docker socket access are high-risk operational
choices required for local Docker Compose validation. Changes that expand
agent filesystem, network, Docker, registry, or host access MUST name the
threat model impact and mitigation. Remote MPFS use MUST remain documented as
a trust decision because Moog signs transactions produced by that boundary.

Rationale: The services can spend funds, consume rate limits, publish private
registry artifacts, and run untrusted test assets locally. Security rules are
part of the product contract, not deployment afterthoughts.

### VI. Haskell, Nix, And CLI Quality

Production code MUST follow the repository's existing Haskell architecture:
typed command GADTs, `opt-env-conf` parsing for CLI/env/config-file inputs,
explicit module exports, canonical JSON rendering for command results, and
Reader/effect boundaries for MPFS submission and validation. New abstractions
MUST extend existing local patterns unless the specification justifies a
replacement.

Build and packaging changes MUST respect Cabal, haskell.nix, CHaP, the pinned
compiler, Docker image definitions, tarball outputs, formatter configuration,
and the existing warning profile. Operator-facing CLI help, environment
variables, config-file keys, Docker image entry points, and Compose examples
are public interfaces and MUST be updated together with behavior changes.

Rationale: Moog's correctness depends on making commands, configuration, and
protocol outputs explicit. The current Haskell/Nix shape is deliberate and
keeps binaries, containers, tests, and docs aligned.

### VII. Verification Mirrors Risk

Pure serialization, encryption, validation, parser, and state-transition logic
MUST be covered by unit tests using the existing Hspec, QuickCheck, EGen, and
MockMPFS style where applicable. Changes crossing MPFS, GitHub, wallet,
Docker, email, Antithesis, or packaging boundaries MUST add or update the
strongest feasible integration, E2E, canary, smoke, or operator verification
evidence.

Completion claims MUST cite fresh command evidence from the current tree. If
credentials or infrastructure prevent integration, E2E, or live-boundary
verification, the PR MUST record the limitation and MUST NOT claim equivalent
coverage from unit tests alone.

Rationale: Parser-only confidence is not enough for a tool that signs
transactions, pulls GitHub assets, starts Docker Compose projects, pushes to
Antithesis, and runs as long-lived infrastructure.

### VIII. Documentation And Deployment Are Product Surface

User manuals, ops guides, security notes, troubleshooting docs, deployment
Compose files, Nix packages, Docker images, GitHub workflows, and command
examples MUST stay consistent with code behavior. A change to role semantics,
configuration, secrets, deployment layout, token lifecycle, test-run lifecycle,
or external service assumptions MUST update the relevant docs and deploy
artifacts in the same PR unless the PR records a maintainer-approved deferral.

Documentation MUST distinguish current implementation from future intent.
Examples that name Cardano Foundation defaults, public MPFS hosts, Antithesis
tenant values, or `/secrets` layouts MUST either remain correct for shipped
deployment files or be clearly marked as illustrative.

Rationale: Operators experience Moog through docs, Compose files, service
logs, and CLI help as much as through library code. Stale deployment guidance
is a production defect.

### IX. Specification Precedes Implementation

Behavior-changing work MUST be issue-backed and represented by explicit
specification, plan, and task artifacts before implementation begins, unless a
human maintainer explicitly scopes the change as documentation-only or
one-line maintenance. The artifacts MUST state the primary user story,
acceptance criteria, non-goals, owned paths, slice boundaries, external
boundaries, security implications, and verification commands.

Rationale: Spec-first development keeps Cardano, MPFS, Antithesis, GitHub,
Docker, wallet, and deployment assumptions visible before code changes make
them expensive to correct.

### X. Workflow Governance And Reviewable History

When a workflow skill or repository process defines roles, phases, stop
points, or delegation rules, those rules are mandatory. An orchestrator MUST
NOT perform work assigned to a driver, reviewer, or release operator. If a
required workflow tool, pane, credential, or context is unavailable, the agent
MUST stop and report the blocker instead of silently substituting a different
workflow.

Issue-backed PR history MUST remain reviewable. Behavior-changing slices MUST
be independently reviewable and bisect-safe, and task completion MUST be tied
to the commit that completes the task.

Rationale: Moog development changes operator-facing behavior and live service
paths. Reviewable history and role discipline are how maintainers can trust
agent-assisted changes.

## Development Workflow

All issue-backed implementation work follows this sequence:

1. Refresh the canonical base from `origin/main`.
2. Read the issue, linked issues, current PR state, and affected code paths.
3. Produce or update `spec.md`, `plan.md`, and `tasks.md`.
4. Review cross-artifact consistency before implementation.
5. Dispatch behavior-changing slices only through the agreed worker workflow.
6. Review each slice diff, run the gate, mark matching tasks, and amend task
   completion into the slice commit.
7. Keep the PR draft until every task is complete and the final gate is green.

The orchestrator owns specifications, plans, tasks, worker briefs, PR
metadata, and verification review. Workers own behavior-changing
implementation inside the paths assigned by their brief. This boundary is
mandatory unless a human maintainer explicitly changes the workflow for the
current ticket.

## Quality Gates

Every behavior-changing PR MUST define a branch gate before implementation.
For this repository, the default local gate is:

```bash
git diff --check
nix develop --quiet -c cabal build all --enable-tests
nix develop --quiet -c cabal test unit-tests --test-show-details=direct
nix develop --quiet -c cabal-fmt -c moog.cabal CI/rewrite-libs/rewrite-libs.cabal
nix develop --quiet -c fourmolu -m check src app test CI/rewrite-libs
nix develop --quiet -c hlint -c src app test CI/rewrite-libs
```

Boundary-specific changes MUST extend that gate or the task verification with
the relevant command: MPFS canary or integration tests for MPFS changes,
GitHub integration tests for GitHub validation changes, E2E scenarios for
role protocol changes, Docker image smoke tests for packaging changes, and
Compose or operator command recovery for deployment changes.

If full CI, integration, E2E, or live-boundary checks require unavailable
operator credentials or infrastructure, the PR MUST record the limitation and
the strongest completed local evidence. It MUST NOT claim equivalent coverage.

## Governance

This constitution governs repository purpose, protocol behavior, security
posture, operator-facing deployment, issue-backed development, and
agent-assisted workflow in this repository. Pull requests that conflict with a
MUST-level rule require either a constitution amendment or an explicit
maintainer-approved exception documented in the PR.

Amendments require:

1. A PR that updates this file.
2. A Sync Impact Report at the top of this file.
3. Review of affected spec, plan, task, and workflow templates if they exist.
4. A semantic version bump:
   - MAJOR for incompatible governance changes or principle removals.
   - MINOR for new principles or materially expanded obligations.
   - PATCH for clarifications that do not change obligations.

Version: 0.2.0
Ratified: 2026-05-25
Last Amended: 2026-05-25
