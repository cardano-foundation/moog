<!--
Sync Impact Report
Version change: none -> 0.1.0
Modified principles: none; initial constitution
Added sections:
- Core Principles
- Development Workflow
- Quality Gates
- Governance
Removed sections: none
Templates requiring updates:
- ⚠ pending: .specify/templates/plan-template.md (template directory not present)
- ⚠ pending: .specify/templates/spec-template.md (template directory not present)
- ⚠ pending: .specify/templates/tasks-template.md (template directory not present)
- ⚠ pending: .specify/templates/commands/*.md (template directory not present)
Follow-up TODOs:
- TODO(SPEC_KIT_TEMPLATES): decide whether this repo should adopt full
  Spec Kit templates or keep only the constitution artifact.
-->

# Moog Constitution

## Core Principles

### I. Workflow Contracts Are Binding

When a workflow skill or repository process defines roles, phases, or stop
points, those rules are mandatory. An agent acting as an orchestrator MUST NOT
perform work assigned to a driver, reviewer, or release operator. If a required
workflow tool, pane, credential, or context is unavailable, the agent MUST stop
and report the blocker instead of silently substituting a different workflow.

Rationale: Moog development often touches operator-facing commands and live
deployment paths. Substituting a local shortcut for the agreed process produces
unreviewable work and breaks trust in the artifact chain.

### II. Specification Precedes Implementation

Behavior-changing work MUST be issue-backed and represented by explicit
specification, plan, and task artifacts before implementation begins, unless a
human maintainer explicitly scopes the change as documentation-only or
one-line maintenance. The artifacts MUST state the P1 user story, acceptance
criteria, non-goals, owned paths, slice boundaries, and verification commands.

Rationale: Spec-first development keeps Cardano, Antithesis, and deployment
assumptions visible before code changes make them expensive to correct.

### III. Test-First Slices And Evidence-Based Verification

Each behavior-changing slice MUST be independently reviewable, bisect-safe, and
covered by a failing test or documented RED-skip rationale before GREEN work
starts. Completion claims MUST cite fresh command evidence from the current
tree. Shipped CLI and operator command changes MUST include command-recovery
proof that exercises the operator-facing surface, not only internal helpers.

Rationale: Parser-only confidence is not enough for an operator command. Moog
coordinates wallets, MPFS, Docker, GitHub, and Antithesis, so proof must match
the boundary the operator depends on.

### IV. Explicit, File-Backed Operations

Deployment configuration MUST be explicit, documented, and fail closed when a
required tenant, registry, token, wallet, or MPFS setting is absent. Secrets and
runtime settings SHOULD be file-backed through stable mounted paths when that
reduces operational rotation risk. Hard-coded tenant-specific values are not
allowed in shipped operator paths unless the path is deliberately test-only and
the name makes that clear.

Rationale: Moog is operated across live deployment boundaries. Operators must
be able to rotate credentials and tenant settings without hidden source defaults
or ad hoc Compose edits.

### V. Haskell And Nix Quality Is The Baseline

Production Haskell code MUST follow the repository's existing module style,
explicit exports, formatter configuration, and Nix/Cabal workflow. Shared
parser, CLI, MPFS, wallet, and Antithesis behavior MUST prefer existing local
abstractions over new parallel mechanisms. Dependency, formatting, and build
changes MUST be justified by the slice that needs them.

Rationale: Moog is small enough that local consistency matters more than
inventing new layers. The safest changes extend the existing parser, command,
and validation patterns.

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

The orchestrator owns specifications, plans, tasks, worker briefs, PR metadata,
and verification review. Workers own behavior-changing implementation inside
the paths assigned by their brief. This boundary is mandatory unless a human
maintainer explicitly changes the workflow for the current ticket.

## Quality Gates

Every behavior-changing PR MUST define a branch gate before implementation.
For this repository, the default gate is:

```bash
git diff --check
nix develop --quiet -c cabal build all --enable-tests
nix develop --quiet -c cabal test unit-tests --test-show-details=direct
nix develop --quiet -c cabal-fmt -c moog.cabal CI/rewrite-libs/rewrite-libs.cabal
nix develop --quiet -c fourmolu -m check src app test CI/rewrite-libs
nix develop --quiet -c hlint -c src app test CI/rewrite-libs
```

If full CI, integration, E2E, or live-boundary checks require unavailable
operator credentials or infrastructure, the PR MUST record the limitation and
the strongest completed local evidence. It MUST NOT claim equivalent coverage.

## Governance

This constitution governs issue-backed development, operator-facing changes,
and agent-assisted workflow in this repository. Pull requests that conflict
with a MUST-level rule require either a constitution amendment or an explicit
maintainer-approved exception documented in the PR.

Amendments require:

1. A PR that updates this file.
2. A Sync Impact Report at the top of this file.
3. Review of affected spec, plan, task, and workflow templates if they exist.
4. A semantic version bump:
   - MAJOR for incompatible governance changes or principle removals.
   - MINOR for new principles or materially expanded obligations.
   - PATCH for clarifications that do not change obligations.

Version: 0.1.0
Ratified: 2026-05-25
Last Amended: 2026-05-25

