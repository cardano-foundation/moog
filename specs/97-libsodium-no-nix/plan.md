# Plan — Repair libsodium fork install in no-Nix build

- **Issue:** [#97](https://github.com/cardano-foundation/moog/issues/97)
- **Spec:** [`spec.md`](spec.md)
- **Research:** [`research.md`](research.md)
- **Phase:** Plan
- **Recommendation:** Option A from research — set
  `DO_NOT_UPDATE_CONFIG_SCRIPTS=1` on the `Install libsodium fork`
  step.

## Orchestrator / subagent ownership

- **Orchestrator (this thread):** owns `spec.md`, `research.md`,
  `plan.md`, `tasks.md`, `./gate.sh`, the PR body, the cache-clear
  step before live proof, the workflow run URL, the finalization
  audit, and the post-merge cleanup.
- **Subagent (one short-lived implementation worker):** owns
  exactly `.github/workflows/build-no-nix.yaml` for one
  bisect-safe `fix(ci):` commit.

The subagent does not load this skill or any other process skill;
its brief in `tasks.md` is the contract.

## Slice plan

One slice, one subagent run, one commit — this PR fits the smallest
viable unit.

### Slice 1 — `fix(ci): skip libsodium autogen.sh config-script download`

| | |
|---|---|
| **Task id** | `T001` |
| **Type** | Behavior change (CI workflow) |
| **Owned files** | `.github/workflows/build-no-nix.yaml` |
| **Change** | Add `DO_NOT_UPDATE_CONFIG_SCRIPTS: 1` to the `env:` of the `Install libsodium fork` step, and prepend a short comment explaining why. |
| **Commit subject** | `fix(ci): skip libsodium autogen.sh config-script download` |
| **Tasks trailer** | `Tasks: T001` |
| **Bisect-safe?** | Yes — single-file, single-step, no cross-file invariants; reverting restores the failing pre-fix behavior. |

### Diff shape

```yaml
      - name: Install libsodium fork
        if: steps.libs-cache.outputs.cache-hit != 'true'
+       env:
+         # autogen.sh otherwise curls config.guess/config.sub from
+         # git.savannah.gnu.org/gitweb without --fail; when that CGI
+         # is slow or 5xx the HTML lands in build-aux/config.sub and
+         # ./configure dies with "cannot run /bin/bash ./build-aux/config.sub".
+         # The fork commits both files in tree; this env var is the
+         # documented escape hatch in autogen.sh itself.
+         DO_NOT_UPDATE_CONFIG_SCRIPTS: 1
        run: |
          mkdir -p ~/src
          cd ~/src
          SODIUM_VERSION=$(curl ...)
          echo "Using sodium version: $SODIUM_VERSION"
          git clone https://github.com/intersectmbo/libsodium
          cd libsodium
          git checkout $SODIUM_VERSION
          ./autogen.sh
          ./configure --prefix=$HOME/.local
          make
          make install
```

No other steps change. No cache-key change. No Nix-path touch.

## Proof strategy

### RED (the failure under repair)

Workflow CI fixes have no local unit-test surface that can fail. The
RED proof for this slice is the *existing* failing workflow run cited
in the issue:

- [Run 26026528215, job 76501180598](https://github.com/cardano-foundation/moog/actions/runs/26026528215/job/76501180598)
  — fails at `Install libsodium fork` with
  `configure: error: cannot run /bin/bash ./build-aux/config.sub`.

This is documented in `spec.md` and `research.md` as the failure
mode. The reviewed slice flips this to GREEN at the GitHub Actions
runner boundary; see "Live-boundary diagnostic" below.

A local *simulated* RED (clone the fork in a sandbox with savannah
blocked, observe `./configure` fail without the env var, then re-run
with the env var and observe success) is **possible** but not added
to `./gate.sh` because:

- It would shell out to network operations and a 1-min libsodium
  build for every gate run, slowing the orchestrator and subagent
  loop disproportionately for a one-line CI change.
- It re-proves the autogen.sh source already inspected in
  `research.md`; we already know which line of the script the env
  var disables.
- The real boundary being repaired is the GitHub-hosted runner +
  `actions/cache` interaction, which a local simulation cannot
  exercise; only the live workflow run can (see below).

### GREEN — local

The reviewed commit must keep `./gate.sh` green at HEAD:

- `git diff --check`
- `nix develop --quiet -c just build`
- `nix develop --quiet -c just unit`
- `nix develop --quiet -c cabal-fmt -c moog.cabal CI/rewrite-libs/rewrite-libs.cabal`
- `nix develop --quiet -c fourmolu -m check src app test CI/rewrite-libs`
- `nix develop --quiet -c just hlint`
- `nix shell nixpkgs#actionlint --quiet -c actionlint -shellcheck '' .github/workflows/build-no-nix.yaml`

The last item proves the edited workflow remains valid YAML +
valid GitHub Actions schema; the rest re-run unchanged because
this PR touches no Haskell source. The subagent must `./gate.sh`
before returning the commit; the orchestrator re-runs it on review.

### GREEN — live (operator follow-up, see below)

A successful `Build moog without nix` workflow_dispatch run on this
branch, on a cleared `~/.local` cache, reaching `cabal build all`.

## Live-boundary diagnostic

*"What system boundary does this slice exercise that the unit suite
cannot?"*

- **The GitHub Actions runner boundary** — a clean `ubuntu-latest`
  image, `actions/cache@v5` miss path, `./autogen.sh` actually being
  executed by the workflow under that env var.
- **The `actions/cache@v5` ↔ `Install libsodium fork` interaction**
  — only an actual cache-miss run on the branch proves that, on a
  fresh runner, the step still produces a sodium install under
  `$HOME/.local` and that `pkg-config` later finds it.

Per the `live-boundary-smoke` skill, the slice therefore must be
backed by either a live smoke in `./gate.sh` or a documented
operator follow-up with a named owner and a verifiable artifact.

**Choice for this slice: documented operator follow-up** (not a
smoke in `./gate.sh`). Reasoning:

- The local simulation cost (cloning the fork, building libsodium,
  blocking network) is high for a one-line edit and re-proves
  source already inspected in research.
- The boundary being exercised is GitHub Actions + cache, which a
  local shell smoke cannot reproduce. A simulation that passes
  locally but fails on the runner would teach us nothing new.
- A `workflow_dispatch` run on the branch is *the* canonical proof
  for this kind of change and yields a public run URL we attach
  to the PR.

### Operator follow-up contract

- **Owner:** orchestrator (this resolve-ticket flow).
- **When:** between subagent acceptance and `gh pr ready`.
- **Steps:**
  1. Invalidate the cache so the libsodium install step actually
     runs:
     ```bash
     gh cache delete Linux-libs --repo cardano-foundation/moog
     ```
     (key is `${{ runner.os }}-libs` from the workflow; matches
     `Linux-libs` on `ubuntu-latest`.)
  2. Trigger the workflow on this branch:
     ```bash
     gh workflow run "Build without nix" \
       --repo cardano-foundation/moog \
       --ref fix/libsodium-no-nix-build
     ```
  3. Watch the run; expect `Install libsodium fork` to complete
     and the job to reach `Cabal build all`.
  4. Paste the run URL into the PR body under "Live proof".
- **Verifiable artifact:** the public run URL on
  `github.com/cardano-foundation/moog/actions/runs/<id>`,
  showing `Install libsodium fork` green and `Cabal build all`
  reached. PR stays draft until this lands.

If the run fails for an *unrelated* reason after `Install
libsodium fork` (e.g., a transient `cabal` solver hiccup), the
follow-up is to rerun, not to broaden this PR's scope.

## Risks and edge cases

| Risk | Mitigation |
|---|---|
| Committed `build-aux/config.sub` in the fork doesn't cover a newer ubuntu-latest arch. | For x86_64 ubuntu-latest today the file is fine. If GitHub later adds a new arch, the failure would be an explicit "unsupported arch" message, not a silent corruption. Recovery: bump iohk-nix's sodium pin (normal flow) or override the file once. |
| Cache key `Linux-libs` is unversioned, so a previously-cached install (from before this PR) hides the fix from cache-hit runs. | This is exactly why the operator follow-up step #1 invalidates the cache before live proof. Future-proofing the key is a noted follow-up (see Out of scope). |
| The env var is read by `autogen.sh` but not by `./configure` itself. | Confirmed by direct source inspection in `research.md`. `configure` is unaffected. |
| Adding `env:` to the step accidentally shadows existing inherited env. | The step previously had no `env:` block; this PR is the first to introduce one. No regression possible. |
| Some other no-Nix CI failure surfaces after this fix. | Per spec non-goals, do not broaden the ticket; file a separate issue. |

## Migration concerns

None. The change is a workflow-only edit that affects only the no-Nix
build, which is currently non-required. No external consumer depends
on the behavior we change.

## Out of scope (carry-forward from spec)

- Nix build path / `haskell.nix` configuration.
- Migration off the Cardano libsodium fork.
- Flipping `Build moog without nix` back to required-check status.
- Pre-existing `actionlint -shellcheck` info-level findings on
  `build-no-nix.yaml`.
- Cache-key versioning (embedding `SODIUM_VERSION` in the key so a
  future fork-pin bump auto-invalidates the cache). Out of scope
  unless the operator follow-up reveals it is required for FR1–FR4;
  if not, file as a follow-up.
- Upstreaming a fix to `intersectmbo/libsodium`'s `autogen.sh` (e.g.,
  `curl --fail`).
- The same fix on `cardano-foundation/developer-portal`'s
  `installing-cardano-node.md` (cross-repo follow-up).

## Tasks-phase preview

Exactly one task. Single subagent dispatch. `tasks.md` will carry the
full subagent brief — owned file list, RED/GREEN expectations,
commit subject, `Tasks: T001` trailer requirement, and the explicit
note that the live-boundary proof is a post-acceptance orchestrator
step, not the subagent's responsibility.
