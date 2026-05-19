# Tasks — Repair libsodium fork install in no-Nix build

- **Issue:** [#97](https://github.com/cardano-foundation/moog/issues/97)
- **Spec:** [`spec.md`](spec.md)
- **Research:** [`research.md`](research.md)
- **Plan:** [`plan.md`](plan.md)
- **Phase:** Tasks

This PR ships one implementation slice, dispatched to one short-lived
subagent. The orchestrator owns the live-boundary proof (cache delete
+ workflow_dispatch) after the subagent's commit is accepted, before
`gh pr ready`.

## Task list

- [X] **T001** — `fix(ci): skip libsodium autogen.sh config-script download`
      — subagent slice (behavior change). Brief below.
- [ ] **T002** — *Orchestrator-only*, post-acceptance. Delete the
      `Linux-libs` cache and trigger `Build without nix` on this branch
      to produce the live-boundary proof run URL. Attach to PR body.
      No commit; this is verification work.
- [ ] **T003** — *Orchestrator-only*, finalization. Drop `gate.sh`
      (`chore: drop gate.sh (ready for review)`) and `gh pr ready 100`.
      No subagent.
- [X] **T004** — *Controller follow-up*, discovered by T002 and resolved
      by rebasing onto current `origin/main`. The base branch now pins
      `lambdasistemi/cardano-mpfs-offchain` to a reachable revision, so
      a clean no-Nix runner can fetch it during `cabal build all`.
- [X] **T005** — *Controller follow-up*, discovered by the T004 live
      run. Add `liblzma-dev` to the no-Nix APT package set so
      `pkg-config` can satisfy `lzma:+pkgconfig` during Cabal
      dependency resolution.
- [X] **T006** — *Controller follow-up*, discovered by the T005 live
      run. Add `librocksdb-dev` to the no-Nix APT package set so
      `rocksdb-haskell-jprupp` can configure against the RocksDB C
      library.

T001, T005, and T006 are the behavior-changing commits in this branch.
T004 is resolved by the current base branch. T002 and T003 are
orchestrator verification + finalization, not subagent work, so the
`Tasks:` trailers in the slice commits list their respective task ids only.
The T002 / T003
checkboxes are flipped by the orchestrator as part of finalization
audit (not amended into a code commit, since they ship no code).

## Subagent brief — T001

```text
Task: T001

Context:
- You are not alone in the codebase. Do not revert edits made by others.
- Make exactly ONE commit. Do not push.
- This commit must be bisect-safe and vertical: building the commit at
  HEAD must succeed; ./gate.sh must pass; no WIP/draft/tmp/fixup/squash
  commits.
- Commit subject must match Conventional Commits:
  `<type>(<scope>)?: <description>`
  where <type> is one of: feat, fix, docs, test, refactor, perf, build,
  ci, chore, style, revert. The commit body must be non-empty.
- The commit body MUST include the trailer `Tasks: T001` on its own
  line, naming the single task this commit closes. This is the durable
  commit ↔ task link. The orchestrator will mark T001's checkbox in
  tasks.md and amend this HEAD commit during review; do not create a
  separate task-stamping commit.
- Maintain ./WIP.md in the worktree root (gitignored) as an append-only
  run log. Add a timestamped entry every time you achieve something —
  do not batch. Required milestones for this slice:
    * brief received (task id, owned files)
    * pre-change actionlint baseline observed (green expected)
    * edit applied (one-line summary of the diff)
    * ./gate.sh run (pass/fail + log path or tail)
    * commit created (SHA + subject)
    * any blocking failure or scope question
  Entry format per milestone:
    ## <ISO-8601 timestamp> — <milestone label>
    <1–4 lines of detail>
  Do not delete or rewrite earlier entries within this run.

Owned files:
- .github/workflows/build-no-nix.yaml  (this slice's ONLY file)

Forbidden scope (do not edit):
- specs/                       (orchestrator owns spec/research/plan/tasks)
- gate.sh                      (orchestrator owns it)
- .gitignore                   (already configured)
- README.md, justfile, flake.nix, nix/*, cabal.project, *.cabal
- Any other .github/workflows/*.yaml
- Any source file under src/, app/, test/, test-integration/, CI/
- PR / issue metadata (no gh commands)

Required orchestrator analysis already applied (see research.md and
plan.md, do NOT re-investigate):
- Root cause: intersectmbo/libsodium's autogen.sh unconditionally does
  `curl -sL` (no --fail) against git.savannah.gnu.org/gitweb to
  overwrite build-aux/config.{guess,sub}. When the CGI is slow / 5xx,
  an HTML error page lands at config.sub and ./configure dies with
  "cannot run /bin/bash ./build-aux/config.sub".
- Fix: set the documented escape hatch `DO_NOT_UPDATE_CONFIG_SCRIPTS=1`
  on the `Install libsodium fork` step so autogen.sh skips the curl
  block. The fork already commits build-aux/config.{guess,sub} in tree.
- Surface: add an `env:` block on the existing step:
    - name: Install libsodium fork
      if: steps.libs-cache.outputs.cache-hit != 'true'
      env:
        # <comment block explaining WHY — see below>
        DO_NOT_UPDATE_CONFIG_SCRIPTS: 1
      run: |
        ...unchanged...
- Comment shape (verbatim is fine, paraphrase OK as long as it conveys
  the WHY and points the next reader to research.md):
    # autogen.sh in the libsodium fork overwrites
    # build-aux/config.{guess,sub} by curl-ing
    # git.savannah.gnu.org/gitweb without --fail; when that CGI is
    # slow or returns 5xx the HTML error page lands in config.sub
    # and ./configure fails with
    #   "cannot run /bin/bash ./build-aux/config.sub"
    # The fork commits both files in tree; this env var is the
    # documented escape hatch in autogen.sh itself.
    # See specs/97-libsodium-no-nix/research.md for the full
    # investigation.
- Do NOT touch: the cache step, the cache key, blst step, secp256k1
  step, cabal step, any other workflow file, any Haskell source.
- Do NOT bump any pinned version (IOHKNIX_VERSION, SODIUM_VERSION,
  BLST_VERSION, SECP256K1_VERSION, etc.).
- Do NOT add `cabal-fmt` / `fourmolu` / `hlint` overrides; the
  inherited gate already runs them.

RED proof (NOT a local test for this slice):
- The RED is the existing failing run already documented:
    https://github.com/cardano-foundation/moog/actions/runs/26026528215/job/76501180598
- The boundary being repaired is the GitHub Actions runner +
  actions/cache, which no local shell can reproduce. The orchestrator
  produces the live-boundary GREEN evidence (workflow_dispatch on this
  branch with the Linux-libs cache deleted) after accepting your
  commit. You do NOT need to invalidate the cache, run gh workflow run,
  or attach a workflow URL — that is T002 and is orchestrator-only.
- What you DO need to confirm locally before the edit:
    nix shell nixpkgs#actionlint --quiet -c \
      actionlint -shellcheck '' .github/workflows/build-no-nix.yaml
  must be green on baseline (it already is). Record the result in
  WIP.md as "pre-change actionlint baseline observed".

GREEN proof — exact commands the worker must run after the edit:
- nix shell nixpkgs#actionlint --quiet -c \
    actionlint -shellcheck '' .github/workflows/build-no-nix.yaml
  (must remain green; this catches YAML/Actions-schema regressions
  introduced by your edit.)
- ./gate.sh
  (the full inherited Haskell + actionlint gate; must be green.)

Commit subject (use this exact title):
- fix(ci): skip libsodium autogen.sh config-script download

Commit body should:
- Explain the WHY in two short paragraphs (root cause + escape hatch),
  pointing readers to specs/97-libsodium-no-nix/research.md for the
  full investigation. Keep it concise; do not duplicate research.md.
- Reference issue #97 (e.g. "Refs #97" or "Part of #97"). Do NOT
  use "Closes #97" — closing is done by the merge of this PR.
- End with the trailer line on its own:
    Tasks: T001

Report back to the orchestrator (plain text reply, not a commit):
- changed files (expect exactly one: .github/workflows/build-no-nix.yaml)
- pre-change actionlint baseline (single line, pass/fail)
- post-change actionlint result (single line, pass/fail)
- ./gate.sh tail (last ~10 lines or "all steps green")
- new commit SHA + subject line
- pointer to ./WIP.md (the orchestrator reads it alongside the diff)
- residual risks (likely "none — see plan.md risk table")

If you discover any contradiction with research.md or plan.md, or
need to touch a file outside the owned set, STOP and report rather
than improvising. The orchestrator will update the brief and
redispatch.
```

## Folding RED + GREEN into one commit

For this slice, RED is not a separate local test commit — see "RED
proof" in the brief. The reviewed commit *is* the GREEN; the RED is
the pre-existing remote failing run cited in spec/research/plan and
linked in the commit body. No folding is needed because there are no
separate RED/GREEN tasks to merge.

## What the orchestrator does after subagent acceptance

1. Inspect the diff and ./WIP.md; rerun `./gate.sh` locally.
2. Run the commit message gate over the new commit (Conventional
   Commits + `Tasks: T001` trailer).
3. Amend HEAD to mark T001's checkbox in this file `[X]`. No
   separate task-stamping commit.
4. Push.
5. **T002 — Live-boundary proof (orchestrator):**
   - `gh cache list --repo cardano-foundation/moog`
     → confirm `Linux-libs` exists (skip delete if already absent).
   - `gh cache delete Linux-libs --repo cardano-foundation/moog`
     (best-effort; if already deleted, continue).
   - `gh workflow run "Build without nix" \
        --repo cardano-foundation/moog \
        --ref fix/libsodium-no-nix-build`
   - Watch run with `gh run watch` / `gh run view`.
   - Expect `Install libsodium fork` green and `Cabal build all`
     reached.
   - Paste the run URL into the PR body under a "Live proof" heading
     and mark T002's checkbox `[X]` (orchestrator amends nothing —
     this checkbox flip is part of finalization audit on a
     separate `docs:` commit alongside any PR-body alignment).
6. **T003 — Finalization (orchestrator):**
   - Run `finalization_audit 100` from the `gate-script` skill.
   - `git rm gate.sh && git commit -m "chore: drop gate.sh (ready for review)"`.
   - Push, then `gh pr ready 100`.
   - Mark T003's checkbox `[X]`.

## Out of scope (carry-forward from plan)

- Cache-key versioning (embedding SODIUM_VERSION in the key).
- Upstream patch to the libsodium fork's autogen.sh.
- Same fix on cardano-foundation/developer-portal docs.
- Reconsider `Build moog without nix` required-check status (handled
  separately once the workflow is reliably green).
