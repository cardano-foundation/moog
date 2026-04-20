<!--
Sync Impact Report
==================
Version change: (none) → 1.0.0
Ratification: initial adoption 2026-04-20.
Modified principles: N/A (first ratification)
Added sections:
  - Core Principles (I–V)
  - Supply Chain & Dependencies
  - Development Workflow & Quality Gates
  - Governance
Removed sections: none.
Templates requiring updates:
  - ✅ .specify/templates/plan-template.md — Constitution Check aligns (generic gate, principles apply)
  - ✅ .specify/templates/spec-template.md — no changes required (principles are project, not spec-shape constraints)
  - ✅ .specify/templates/tasks-template.md — no changes required (existing TDD/integration categories remain applicable)
  - ✅ .claude/commands/speckit.*.md — no rewrites needed; commands reference constitution generically
Follow-up TODOs: none.
-->

# Moog Constitution

Moog is an oracle that reads off-chain events and publishes them on-chain via the
MPFS protocol. The constitution below governs all work on Moog, including the
pending migration of Moog's MPFS client from the deprecated TypeScript service
(`cardano-foundation/mpfs`) to the Haskell service
(`lambdasistemi/cardano-mpfs-offchain`).

## Core Principles

### I. Duplication-First Migration (NON-NEGOTIABLE)

When adopting a new upstream service whose types are technically importable,
Moog MUST first migrate with **locally duplicated types** that match the new
wire format, and only later consider replacing duplicates with direct upstream
imports. The duplication is intentional: it preserves Moog's independence from
the upstream dependency graph and its release cadence, and it keeps the
migration diff focused on wire-format and call-site changes rather than on a
concurrent dependency refactor.

Rationale: concurrent "adopt new server + import its types" is a double change
that is hard to bisect and hard to revert; separating them limits blast radius.
A later refactor (to import) is a deliberate, second decision — not a
side-effect of the migration.

### II. Lean Dependency Tree

Moog MUST NOT pull heavy Cardano ecosystem dependencies (in particular
`cardano-ledger-*`, `ouroboros-*`, `rocksdb`, Plutus compiler) transitively
through its MPFS client. The MPFS wire protocol is HTTP+JSON; Moog talks to
MPFS **over the network**, and its client code MUST reflect that boundary.

Concretely:
- Moog's MPFS client MUST depend only on `http-client`, `servant-client`,
  `aeson`, and equivalents — not on `cardano-mpfs-offchain` as a Haskell
  library dependency.
- If Moog needs proof verification, it MAY depend on `cardano-mpfs-client`
  (the slim verifier package) once that package is published to CHaP or
  pinned via `source-repository-package`.
- Adding `cardano-ledger-*` or similar to `moog.cabal` requires an explicit
  constitution amendment or a scoped exception with written rationale in the
  PR description.

Rationale: Moog is an oracle, not a chain indexer. Heavy deps balloon build
times, complicate Nix pinning, and entangle Moog's release with Cardano
ecosystem churn. The user has explicitly flagged "depending on Cardano for
Moog is not a great choice" — this principle encodes that.

### III. E2E Parity Gate (NON-NEGOTIABLE)

Any MPFS-client migration MUST pass the full Moog E2E test suite against the
new `cardano-mpfs-offchain` server before merge. "Compiles" and "unit tests
pass" are necessary but NOT sufficient. The merge gate is: a running
`cardano-mpfs-offchain` instance + Moog E2E run on preprod or an equivalent
ephemeral environment, with results linked in the PR description.

Rationale: the wire format change is deep (response wrappers, method changes,
path reorganization, field casing). Unit tests exercising Moog's client in
isolation cannot catch server-side semantic divergences; only E2E can.

### IV. Bisect-Safe Commits

Every commit on every branch MUST compile and MUST NOT regress the test
suite of the area it touches. Refactors MUST follow the additive-then-remove
pattern: introduce the new shape alongside the old, migrate call sites, then
delete the old shape — in separate commits, each green.

For the MPFS migration specifically: "swap the wire format" MUST NOT be a
single giant commit. It MUST decompose into: (1) add new request/response
types, (2) add new Servant client, (3) switch call sites module by module,
(4) delete old types. Use StGit to retroactively fix any earlier patch that
breaks this property — never append fixup patches.

Rationale: `git bisect` is the primary debugging tool when a regression is
found weeks later. A single commit that leaves the tree broken destroys that
tool. StGit discipline (from user's global rules) makes the additive-remove
pattern mechanical.

### V. Upstream Drift Tracking

Every type, constant, or wire-format definition in Moog that duplicates a
definition in `cardano-mpfs-offchain` MUST carry a source comment of the
form:

```haskell
-- | Mirror of Cardano.MPFS.HTTP.Types.InsertRequest
-- Upstream: lambdasistemi/cardano-mpfs-offchain@<commit-sha>
-- Last synced: YYYY-MM-DD
```

When upstream changes, the drift is visible in code review (comment goes
stale). A periodic audit script MAY enumerate these comments and diff against
the current upstream HEAD. The audit does not block merges, but stale
comments accumulate visible debt.

Rationale: duplication without tracking becomes silent drift. Tracking
converts "silent drift" into "visible staleness" that shows up in grep and
review.

## Supply Chain & Dependencies

- All `source-repository-package` entries in `cabal.project` MUST pin a
  specific commit and MUST include a nix32 `--sha256:` comment (never SRI
  format; see user's global nix pinning rule).
- Pins MUST target commits on upstream `main`, never branches (user's
  `pins_main_only` rule).
- Dependency bumps are tickets, not drive-by changes — each bump opens its
  own issue and PR.
- The `cardano-mpfs-client` verifier package, if adopted, MUST be pinned via
  `source-repository-package` until it is published to CHaP.

## Development Workflow & Quality Gates

- Every issue MUST go through the Spec Kit SDD workflow:
  `/speckit.specify` → `/speckit.plan` → `/speckit.tasks` → `/speckit.implement`.
  No implementation without a spec and task list.
- `just ci` MUST pass locally before every push. CI is a safety net, not a
  debugging tool — "push to see what happens" is prohibited.
- PRs MUST be labeled, assigned to `paolino`, and their description MUST be
  updated after every push (PR description is a living document).
- `merge-guard` MCP MUST be used in place of `gh pr merge`.
- Main branch is protected; all changes MUST flow through PRs with rebase
  merge (linear history).
- Each branch MUST live in its own git worktree (`<repo>-<short-name>`).
- Haskell formatting gate: `fourmolu` + `cabal-fmt` + `hlint` MUST pass
  before every push (user's `lint_before_push` rule).

## Governance

This constitution supersedes any conflicting local convention in the Moog
repository. It does NOT supersede the user's global CLAUDE.md rules or the
workflow skill — those remain the outermost envelope; this constitution
scopes them to Moog.

Amendment procedure:
1. Propose the amendment as a PR editing `.specify/memory/constitution.md`.
2. Update the Sync Impact Report at the top of the file.
3. Bump `CONSTITUTION_VERSION` per semantic versioning:
   - MAJOR: incompatible removal or redefinition of a principle.
   - MINOR: new principle or materially expanded guidance.
   - PATCH: wording, typo, non-semantic refinement.
4. Propagate changes to any affected templates under `.specify/templates/`.
5. Merge with user approval.

Compliance review: any PR that touches the MPFS client, `moog.cabal`
dependencies, or CI gates MUST explicitly confirm constitutional compliance
in its description (one line per relevant principle, or "N/A" with reason).

**Version**: 1.0.0 | **Ratified**: 2026-04-20 | **Last Amended**: 2026-04-20
