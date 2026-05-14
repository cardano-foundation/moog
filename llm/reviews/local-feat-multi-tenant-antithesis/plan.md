# Plan — issue #92: Support multiple Antithesis tenants

## Spec (from issue)

Antithesis is moving the project from the `cardano` tenant to
`amaru-cardano`. Each tenant has its own launch URL and GAR.
Two values are currently baked into moog and block the move:

1. Launch URL: `https://cardano.antithesis.com/api/v1/launch/cardano`
   hardcoded in `renderPostToAntithesis` (`src/User/Agent/PushTest.hs:246`).
2. Registry default:
   `us-central1-docker.pkg.dev/molten-verve-216720/cardano-repository`
   set as the `--registry` default (`src/User/Agent/Options.hs:189-198`).

Both must follow the same env + secrets.yaml pattern used by
`antithesisPassword` / `MOOG_ANTITHESIS_USER`.

## Design

### Launch URL

- New newtype `LaunchUrl` in `User.Agent.PushTest`.
- New field on `AntithesisAuth` (or sibling record) carrying it.
  Decision: add `launchUrl :: LaunchUrl` to `AntithesisAuth`. The
  three fields (user, password, launch URL) are populated together
  at startup from the same secrets/env source, and `renderPostToAntithesis`
  is the single consumer. A sibling record would add wiring with no
  payoff at this scale.
- `renderPostToAntithesis` reads the URL from `AntithesisAuth`
  instead of the hardcoded literal.
- `antithesisAuthOption` (in `User.Agent.Options`) parses a new
  `launchUrl` field via `env "MOOG_ANTITHESIS_LAUNCH_URL"` +
  `conf "antithesisLaunchUrl"`, no `value` (no default).
  OptEnvConf surfaces a missing required setting as a startup error
  — matches the "fail at startup if missing" requirement.

### Registry

- Drop the in-source default in `registryOption`.
- Add `env "MOOG_REGISTRY"` so the deployment can set it.
- Keep `--registry` flag for ad-hoc invocation.

### Out of scope (per issue)

- `recipients = ["antithesis@cardanofoundation.org"]`
- `From` header in `Email.hs`

## Vertical slices

1. **feat: thread Antithesis launch URL through config**
   - Adds `LaunchUrl` newtype and `launchUrl` field on
     `AntithesisAuth`.
   - Updates `renderPostToAntithesis` to read URL from auth.
   - Wires `launchUrlOption` into `antithesisAuthOption`.
   - Updates `test/User/Agent/PushTestSpec.hs` so the assertion's
     URL comes from the constructed `AntithesisAuth`, proving the
     URL is config-driven (RED → GREEN folded into one commit since
     the function signature must change atomically for the build to
     stay green).

2. **feat: configure Antithesis registry via MOOG_REGISTRY**
   - Adds `env "MOOG_REGISTRY"` to `registryOption`.
   - Drops the in-source `value` default.
   - Existing `PushTestSpec` still passes — it constructs a
     `Registry` value directly, no env/CLI plumbing involved.

3. **docs: document multi-tenant Antithesis config**
   - `docs/user/secrets-management.md`: add `antithesisLaunchUrl`
     to the agent section and the supported-keys table.
   - `docs/ops/deployment.md`: add `MOOG_ANTITHESIS_LAUNCH_URL` and
     `MOOG_REGISTRY` to the env reference and the example compose;
     remove the mention of a registry default.
   - `docs/ops/agent-role.md`: add the two env vars.
   - `CD/moog-agent/docker-compose.yaml`: add commented-out env
     lines for both, and add `antithesisLaunchUrl` to the
     `secrets.yaml` key list comment.

## Risks / edge cases

- **Existing operator state breaks at upgrade**: any deployment
  without `MOOG_ANTITHESIS_LAUNCH_URL`/`antithesisLaunchUrl` set
  will fail at startup. Intentional per the issue — keeps tenants
  explicit. CHANGELOG entry + docs cover this; the only deployment
  affected is the agent host.
- **`conf` vs `secret`**: launch URL is not strictly secret, but the
  issue specifies the secrets.yaml key. Using `conf` aligns with
  `agentEmail` (also non-secret but in secrets.yaml).
- **OptEnvConf "no default" semantics**: relies on OptEnvConf
  refusing to construct the parser result when no source supplies
  the value. Verified by removing `value` from `registryOption` in
  slice 2 — same mechanism for both.

## Proof strategy

- **Slice 1**: `test/User/Agent/PushTestSpec.hs` is the regression
  proof. The "renders the curl command for pushing to Antithesis"
  test builds an `AntithesisAuth` with a specific launch URL and
  asserts that URL appears in the rendered args. If
  `renderPostToAntithesis` ever re-introduces a hardcode, the test
  fails. RED is implicit: the existing test references the old
  hardcoded URL — without the code change, the new test wording
  fails; without the test change, the new signature breaks the
  build.
- **Slice 2**: covered by build + manual review. There is no unit
  test for `registryOption` parsing (consistent with the rest of
  `Options.hs`); the change is mechanical (drop `value`, add `env`).
- **Slice 3**: docs only, no test.

## Gate

`nix develop --quiet -c just CI` locally before pushing.
No live-boundary smoke is added — the curl call is exercised by the
agent integration deployment, not the unit suite, and the issue's
acceptance criteria are met by the unit test + manual deploy.
