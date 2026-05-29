# Feature Specification: Deploy moog-antithesis-proxy

## Primary User Story

As a Moog operator, I need `moog-antithesis-proxy` deployed at
`https://antithesis-proxy.plutimus.com` behind plutimus Traefik so team
members can fetch Antithesis run data without receiving the tenant
password.

## Repository Deliverables

- Produce a `moog-antithesis-proxy` container image target from the same
  Nix/GitHub Actions image pipeline used by the other Moog images.
- Add `docs/antithesis-proxy.compose.example.yaml` as the operator-copyable
  plutimus compose file.
- Add `docs/antithesis-proxy.md` with the deployment shape, commands,
  acceptance checks, and handoff boundary for the fuller #116 runbook.

## Plutimus Deliverables

- Install `/opt/hal/infrastructure/moog/antithesis-proxy/docker-compose.yaml`
  from the example in this PR, pinned to a concrete commit tag.
- Install `/secrets/moog-antithesis-proxy/{old,new}/secrets.yaml` with
  `antithesisPassword` mirroring the agent's Antithesis password.
- Install `/secrets/moog-antithesis-proxy/{old,new}/antithesis-password`
  because the daemon reads `MOOG_ANTITHESIS_PASSWORD_FILE`.
- Point `antithesis-proxy.plutimus.com` A/AAAA records at plutimus.

## Acceptance Criteria

- `curl https://antithesis-proxy.plutimus.com/healthz` returns `200 ok`.
- `curl https://antithesis-proxy.plutimus.com/api/v1/runs` returns `401`
  with `WWW-Authenticate: Bearer realm="moog-antithesis-proxy"`.
- `curl -H "Authorization: Bearer <garbage>" .../api/v1/runs` returns `401`.
- A valid team-member bearer token returns `200` with the Antithesis JSON
  body; #116 owns the full smoke record.
- `docker compose up -d --force-recreate moog-antithesis-proxy` restarts
  the service cleanly on plutimus.
- `docker logs antithesis-proxy-moog-antithesis-proxy-1` shows proxy logs.

## Out Of Scope

- Changing proxy application behavior.
- Running SSH deploy steps from this PR worker.
- The full end-to-end client runbook and smoke evidence, owned by #116.
