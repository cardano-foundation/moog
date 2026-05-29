# Implementation Plan: Deploy moog-antithesis-proxy

## Scope Split

The Moog PR owns reusable artifacts: image packaging, documentation, and an
example compose file. The live deployment is operator-bound and happens on
plutimus after the PR artifacts are ready.

## Image Packaging

`moog-antithesis-proxy` already exists as a cabal executable and Nix package.
The existing `moog-agent` image contains only `moog-agent`, so this ticket adds
a sibling `moog-antithesis-proxy-docker-image` target instead of relying on the
agent image. The GitHub Actions image workflow builds and smoke-tests it in
the same jobs as `moog-agent` and `moog-oracle`.

## Deployment Documentation

`docs/antithesis-proxy.compose.example.yaml` is the source the operator copies
to `/opt/hal/infrastructure/moog/antithesis-proxy/docker-compose.yaml`.
It is pinned by `MOOG_VERSION`, joins the external `web` network, exposes only
container port `8080`, mounts the secret directory read-only, and carries the
requested Traefik labels for `antithesis-proxy.plutimus.com`.

`docs/antithesis-proxy.md` records the architecture, secret layout,
start/stop/log commands, and acceptance checks. The fuller operational
runbook remains in #116.

## Slice

One bisect-safe slice lands all repository-owned artifacts and verification.
The external deployment request is raised as a Q-file after the PR branch is
pushed.
