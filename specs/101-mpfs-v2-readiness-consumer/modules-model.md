# Modules model — 101

| Module | State | Responsibility | Depends on |
|---|---|---|---|
| `MPFS.Readiness` | **new** | Owns the MPFS readiness contract as moog consumes it: which route is probed, what each observed status means, when the gate releases, and how the not-ready evidence is counted and reported. Knows nothing about devnets, genesis, or processes. | HTTP client only |
| `MPFS.Devnet` | changed | Devnet lifecycle: genesis funding, server process start/stop, log capture. Delegates the readiness decision instead of implementing it; keeps process liveness, which is its own concern. | `MPFS.Readiness` |
| `MPFS.Canary` | changed | Full-lifecycle boundary scenario. Carries the readiness evidence into its result. | `MPFS.Readiness` |
| `app/moog-mpfs-v2-canary` | changed | Selects the `/ready` policy and asserts the readiness evidence. | `MPFS.Devnet`, `MPFS.Canary` |
| `test/MPFS/ReadinessSpec` | **new** | Drives the readiness gate through scripted status sequences and their negative controls. | `MPFS.Readiness` |
| `test-integration/Main`, `test-E2E/Main` | S2 only | Consume the same readiness policy; their bespoke `waitForSnapshotReady` helpers are deleted. | `MPFS.Readiness` |

## Promotion rationale

Readiness is promoted to its own module rather than left inside `MPFS.Devnet`
because it has three distinct consumers, is the sole thing under test in this
ticket, and is a different job from process lifecycle. Keeping both jobs in one
helper is what let the contract be re-derived three incompatible ways.

Dependency direction is one-way: lifecycle and scenario depend on readiness;
readiness depends on neither.
