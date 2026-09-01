# Data model — 101

## `ReadinessPolicy`

Which signal a devnet consumer gates on. Two cases for the lifetime of S1 only;
the legacy case is deleted in S2.

| Case | Meaning | Probed route |
|---|---|---|
| `GateOnReady` | the C-SIGNALS dependency gate | `/ready` |
| `GateOnStatusLegacy` | pre-contract behaviour retained so S1 stays bisect-safe | `/status` |

Validation: the probed route is a function of the case and is readable by a
test, so a route change is observable rather than buried in a request builder.

## `ReadinessObservation`

One probe outcome, classified. The vocabulary must keep distinct causes
distinct — collapsing them is the defect R-101-5 exists to prevent.

| Case | Meaning |
|---|---|
| `Ready` | 200 — gate releases |
| `NotReady` | 503 — backend alive, roots not yet correct and current |
| `RouteAbsent` | 404 — backend predates the C-SIGNALS contract |
| `Unreachable` | transport failure — no HTTP answer at all |
| `Unexpected` | any other status, carried with its code |

## `ReadinessOutcome`

Terminal result of the gate.

| Case | Carries | Meaning |
|---|---|---|
| `ReadyAfter` | not-ready observation count | released on 200; the count is the evidence the gate held |
| `ReadyRouteAbsent` | observation count | budget spent seeing only 404 |
| `ReadyTimedOut` | observation count, last observation | budget spent without 200 |
| `ReadyDataRouteUngated` | data route status observed while not ready | C-SIGNALS data-gating violation |

State invariant: `ReadyAfter 0` is a legitimate value of the gate but an
**assertion failure for the canary** — R-101-4 requires the canary to have
observed the gate holding at least once.

## `FullLifecycleCanaryResult` (changed)

Gains one field: the not-ready observation count from `ReadyAfter`, serialized
into the canonical JSON alongside the existing poll counts, so the run's
evidence is machine-readable rather than a log line.
