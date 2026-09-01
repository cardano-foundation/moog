# Functions model — 101

Signature-level only. No bodies, no algorithms.

## `MPFS.Readiness` (new)

| Function | Arguments | Result | Constraints / effects |
|---|---|---|---|
| `readinessProbePath` | `policy :: ReadinessPolicy` | `String` | pure; the route a policy probes. Exists so R-101-1 is assertable without grepping source |
| `classifyReadinessStatus` | `statusCode :: Int` | `ReadinessObservation` | pure; total over all codes |
| `awaitReadiness` | `policy :: ReadinessPolicy`, `maxPolls :: Int`, `delay :: IO ()`, `probe :: IO ReadinessObservation`, `probeDataRoute :: IO (Maybe Int)`, `liveness :: IO (Maybe ExitCode)` | `IO ReadinessOutcome` | effects injected so the decision logic is unit-drivable; never throws for a contract outcome — it returns one |
| `httpReadinessProbe` | `host :: String`, `policy :: ReadinessPolicy` | `IO (IO ReadinessObservation)` | builds the real HTTP probe; the only place a route string is turned into a request |
| `httpDataRouteProbe` | `host :: String` | `IO (IO (Maybe Int))` | samples a data route for R-101-6 |

## `MPFS.Devnet` (changed)

| Function | Arguments | Result | Change |
|---|---|---|---|
| `withDevnetServer` | `tmp :: FilePath`, `cfg :: DevnetConfig`, `genesisDir :: FilePath`, `policy :: ReadinessPolicy`, `action :: String -> IO a` | `IO a` | gains `policy`; delegates the wait to `awaitReadiness` and renders a `ReadinessOutcome` other than `ReadyAfter` into the existing server-log failure |
| `waitForStatus` | — | — | **removed**; replaced by the delegation above |

Callers updated for the new argument: `app/moog-mpfs-v2-canary` (`GateOnReady`),
`test-integration/Main`, `test-E2E/Main` (`GateOnStatusLegacy` in S1, moved to
`GateOnReady` in S2).

## `MPFS.Canary` (changed)

| Function | Arguments | Result | Change |
|---|---|---|---|
| `fullLifecycleCanary` | `client :: MPFSClient`, `wallet :: Wallet`, `maxPolls :: Int`, `readyNotReadyPolls :: Int` | `IO FullLifecycleCanaryResult` | accepts the readiness evidence measured by the gate and records it in the result |

## `app/moog-mpfs-v2-canary` (changed)

| Function | Arguments | Result | Change |
|---|---|---|---|
| `assertCanaryResult` | `result :: FullLifecycleCanaryResult` | `IO ()` | adds the R-101-4 assertion that the not-ready count is at least one |
