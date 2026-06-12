# Repository Agent Guide

## What this repo is

Moog is a Haskell tool to administer [Antithesis](https://antithesis.com)
test execution through Cardano. It builds one CLI (`moog`) and three
services: `moog-oracle` (validates user requests against GitHub and commits
them to an on-chain token through the MPFS service), `moog-agent` (launches
accepted test runs on Antithesis and reconciles their results back
on-chain via the Antithesis REST API), and `moog-antithesis-proxy`
(GitHub-team-authenticated read access to the Antithesis API). A fourth
executable, `moog-mpfs-v2-canary`, is an end-to-end probe for the MPFS v2
migration that is being developed on the `moog-v2` branch.

## How to work here

- Build: `just build` (= `cabal build all --enable-tests`), inside `nix develop`
- Unit tests: `just unit` (or `nix run .#unit-tests` when the dev shell is unavailable)
- Format: `just format` (fourmolu + cabal-fmt + nixfmt); lint: `just hlint`
- Integration/E2E tests need Docker, a funded test wallet and
  `MOOG_GITHUB_PAT` / `MOOG_SSH_PASSWORD` — see the recipe headers in
  `justfile`
- Docs site: MkDocs Material, sources in `docs/`, config in `mkdocs.yml`
- Docker images: `just build-docker-images`; deployment compose files in `CD/`

## Skills

Activatable procedures live under `skills/`. Load the one whose description
matches your task:

- `skills/moog-guide/` — repository guide: code map, build/test/run
  commands, CLI usage, where answers live
- `skills/antithesis-moog/` — production operations runbook: identities,
  secrets layout and rotation, stuck-request recovery, result
  reconciliation debugging
