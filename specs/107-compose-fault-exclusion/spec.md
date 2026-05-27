# Spec — moog#107 compose-driven Antithesis fault exclusion

## P1 user story

As an Antithesis test operator running cardano-node-antithesis through moog, when I mark a docker-compose service in `testnets/<name>/docker-compose.yaml` with `com.antithesis.exclude_from_faults: "network,kill,pause,stop"` (any subset), the test launched by `moog requester create-test` must include that service in the corresponding `custom.*_exclusion` launch params, so that the Antithesis platform skips fault injection on that container — without needing to edit the Antithesis-hosted notebook or any sibling YAML.

## Why

Antithesis's `amaru-cardano.nb2` notebook accepts four launch params, each a comma-separated list of compose service names:

- `custom.network_fault_exclusion`
- `custom.container_faults_kill_exclusion`
- `custom.container_faults_pause_exclusion`
- `custom.container_faults_stop_exclusion`

Today `moog`'s `PostTestRunRequest` hard-codes seven fields and does not emit any of these. The notebook's defaults (`tracer, tracer-sidecar, sidecar`) are the only way to opt a container out of faults, and we don't own the notebook. As a result `tx-generator`, `asteria-game`, and (in the adversary testnet) `adversary` get faulted even though they are workload generators, not parts of the SUT, and their composer scripts have been accumulating "Always: Commands finish with zero exit code" findings as residual after cardano-foundation/cardano-node-antithesis#142 and #145.

Moving the exclusion list onto the compose service itself makes the compose the single source of truth: renames are atomic, the role of each service is readable in one file, and we stop depending on undocumented notebook defaults.

## User stories

- **U1** — As an operator, I can label a service with one or more of `network`, `kill`, `pause`, `stop` and see the matching params appear in the next run's launch payload, comma-joined by service name.
- **U2** — As an operator, when no service in the compose carries a label class, moog must omit the corresponding `custom.*_exclusion` param entirely so the notebook's existing default applies (safe rollout — does not clobber the historical `tracer, tracer-sidecar, sidecar` exclusion until the consumer is ready to take over).
- **U3** — As an operator, if a label value contains a token outside `{network, kill, pause, stop}`, moog fails the launch with a clear error pointing at the service and the offending token, rather than silently dropping the unknown token.

## Functional requirements

- **F1** Parse `<testnet-dir>/docker-compose.yaml` to extract, for each service, the value of the label `com.antithesis.exclude_from_faults`.
- **F2** The label value is a comma-separated list of tokens from the set `{network, kill, pause, stop}`. Whitespace around tokens is permitted. Empty strings between commas are ignored.
- **F3** From the parsed labels, build four sets keyed by class: services whose label includes `network`, services whose label includes `kill`, etc. Order within each set is the order services appear in the compose file (deterministic).
- **F4** A label value containing a token outside the four-element vocabulary is a hard error. The error must name the service and the offending token.
- **F5** Extend `User.Agent.PushTest.PostTestRunRequest` with four `[String]` fields: `networkFaultExclusion`, `containerFaultsKillExclusion`, `containerFaultsPauseExclusion`, `containerFaultsStopExclusion`.
- **F6** `ToJSON PostTestRunRequest` emits `custom.network_fault_exclusion`, `custom.container_faults_kill_exclusion`, `custom.container_faults_pause_exclusion`, `custom.container_faults_stop_exclusion` keys with their lists comma-joined (no spaces), **only when the corresponding list is non-empty**. Empty list = key absent from the payload.
- **F7** `pushTestToAntithesisIO` reads the compose file from the same `Directory context` it already passes to `buildConfigImage`, runs the parser (F1–F4), and populates the four fields on `PostTestRunRequest` before serialization.
- **F8** The change is a no-op for any caller whose testnet directory contains a compose with zero `com.antithesis.exclude_from_faults` labels.

## Success criteria

- **S1** New unit tests cover: empty compose → all four lists empty; one service per class → each class list has that service; one service labeled with all four → all four lists contain it; malformed label → parse error names the service + token.
- **S2** New unit tests cover the `ToJSON PostTestRunRequest` shape: empty list omits the key; populated list emits comma-joined value; mixed empty/populated emits only the populated ones.
- **S3** Existing unit tests still pass without modification — F8 guarantees backwards compatibility.
- **S4** `gate.sh` passes at HEAD on every slice boundary.
- **S5** One manual launch from cardano-foundation/cardano-node-antithesis (post-release of this PR + the corresponding labels PR there) shows `custom.container_faults_*_exclusion` in the run params containing the labeled services.

## Out of scope

- Editing any docker-compose file in any other repository. That is done in the cardano-foundation/cardano-node-antithesis follow-up PR.
- Removing the absorption scaffolding from `components/asteria-game/composer/asteria-game/*.sh`. That is a third, follow-on cleanup once the exclusion is verified in the field.
- Generalizing the label vocabulary beyond the four fault classes the notebook currently accepts.
- Providing CLI overrides for the exclusion lists. Compose is the source of truth; if you want a one-off override, edit the compose temporarily.
