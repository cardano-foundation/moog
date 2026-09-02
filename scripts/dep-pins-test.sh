#!/usr/bin/env bash
# shellcheck shell=bash
# Offline proof for the C-DEP-PINS decision logic (M2-T101 slice SD1).
#
# Sources scripts/dep-pins.sh and drives its decision functions over fixture
# files. fetch_url is overridden with a fixture fetcher, so the whole harness
# runs without the network; the REAL wire is exercised separately and its runs
# recorded as evidence: `just dep-pins` at the baseline (green) and with
# DEP_PINS_OFFCHAIN_REV=0f82465f5f828c2ab987a166e9e24c2368228d01 (red,
# naming 39bf3302...), plus a bogus-revision run proving the fail-closed path.
#
# Every case here is written to be able to fail; the harness exits non-zero if
# any does. Run it via `just dep-pins` (which runs this before the real check).

set -u

# The harness owns its fixture state. The recipe legitimately runs it with an
# ambient DEP_PINS_OFFCHAIN_REV (the gate's negative control does exactly
# that), and an ambient override would leak into every run_check case — so it
# is dropped here; cases that need it set it explicitly.
unset DEP_PINS_OFFCHAIN_REV

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
# shellcheck source=scripts/dep-pins.sh
source "$here/dep-pins.sh"

pass=0
fail=0

ok() { pass=$((pass + 1)); printf 'ok   - %s\n' "$1" >&2; }
ko() { fail=$((fail + 1)); printf 'FAIL - %s\n' "$1" >&2; }

assert_eq() { # <label> <want> <got>
  if [[ "$2" == "$3" ]]; then ok "$1"; else ko "$1 (want $(printf %q "$2"), got $(printf %q "$3"))"; fi
}

assert_match() { # <label> <ERE> <haystack>
  if printf '%s' "$3" | grep -qE -- "$2"; then ok "$1"; else ko "$1 (no match for /$2/)"; fi
}

assert_absent() { # <label> <ERE> <haystack>
  if printf '%s' "$3" | grep -qE -- "$2"; then ko "$1 (unexpected match for /$2/)"; else ok "$1"; fi
}

# --- fixtures ---------------------------------------------------------------

fx=$(mktemp -d)
trap 'rm -rf "$fx"' EXIT

MOOG_ONCHAIN=1111111111111111111111111111111111111111
OFFCHAIN_A=2222222222222222222222222222222222222222
OFFCHAIN_B=3333333333333333333333333333333333333333
DECLARED_BY_B=4444444444444444444444444444444444444444
OFFCHAIN_UNKNOWN=5555555555555555555555555555555555555555
SHORT_REV=6789abcd

# moog cabal.project fixture. With the onchain stanza present the onchain tag
# sits on line 11 and the offchain tag on line 6 — asserted exactly below.
moog_project() { # <onchain-tag|absent|short|duplicated>
  local onchain=$1
  {
    echo 'packages: .'
    echo
    echo 'source-repository-package'
    echo '  type: git'
    echo '  location: https://github.com/lambdasistemi/cardano-mpfs-offchain.git'
    echo "  tag: $OFFCHAIN_A"
    echo
    if [[ "$onchain" != absent ]]; then
      echo 'source-repository-package'
      echo '  type: git'
      echo '  location: https://github.com/cardano-foundation/cardano-mpfs-onchain'
      case "$onchain" in
        short) echo "  tag: $SHORT_REV" ;;
        *) echo "  tag: $onchain" ;;
      esac
      echo '  --sha256: 19m88h0ga5s09vrqqglj0wbwy4knvi9yy4mab9hwdb8mrrfdh5ig'
      echo '  subdir: haskell'
      echo
    fi
    if [[ "$onchain" == duplicated ]]; then
      echo 'source-repository-package'
      echo '  type: git'
      echo '  location: https://github.com/cardano-foundation/cardano-mpfs-onchain-2'
      echo "  tag: $MOOG_ONCHAIN"
      echo
    fi
    echo 'allow-newer: mts:containers'
  } > "$fx/cabal.project"
}

# What the fixture fetcher serves for each offchain revision. The revision the
# offchain fixture declares sits on line 4 — asserted exactly below.
offchain_fixture() { # <name> <declared-onchain-tag> | <name> html | <name> empty
  local name=$1 body=$2
  case "$body" in
    html)
      printf '<html><body>404: Not Found</body></html>\n' > "$fx/$name.project"
      ;;
    empty)
      : > "$fx/$name.project"
      ;;
    *)
      {
        echo 'source-repository-package'
        echo '  type: git'
        echo '  location: https://github.com/cardano-foundation/cardano-mpfs-onchain'
        echo "  tag: $body"
        echo '  --sha256: somehash'
        echo '  subdir: haskell'
      } > "$fx/$name.project"
      ;;
  esac
}

offchain_fixture A "$MOOG_ONCHAIN"   # agrees with moog
offchain_fixture B "$DECLARED_BY_B"  # skewed
offchain_fixture U html              # unparseable response body

# Fixture transport: replaces the real fetch_url so no network is touched.
fetch_url() {
  local url=$1
  case "$url" in
    *"$OFFCHAIN_A"*) cat "$fx/A.project" ;;
    *"$OFFCHAIN_B"*) cat "$fx/B.project" ;;
    *"$OFFCHAIN_UNKNOWN"*) cat "$fx/U.project" ;;
    *)
      printf 'dep-pins-test: fixture fetch has no case for %s\n' "$url" >&2
      return 1
      ;;
  esac
}

# Run the check end-to-end over the current fixture state, in a subshell where
# fetch_url is the fixture fetcher and the CWD is the fixture directory, so the
# check's default input really is ./cabal.project with no ambient override.
# Extra NAME=VALUE arguments become environment for the run (the override
# cases). Sets the caller-visible globals `rc` and `out`.
run_check() {
  local out_file arg
  out_file=$(mktemp)
  (
    unset DEP_PINS_PROJECT_FILE DEP_PINS_OFFCHAIN_REV
    for arg in "$@"; do export "${arg?}"; done
    cd "$fx" || exit 1
    # shellcheck disable=SC2317
    fetch_url() {
      local url=$1
      case "$url" in
        *"$OFFCHAIN_A"*) cat "$fx/A.project" ;;
        *"$OFFCHAIN_B"*) cat "$fx/B.project" ;;
        *"$OFFCHAIN_UNKNOWN"*) cat "$fx/U.project" ;;
        *) return 1 ;;
      esac
    }
    if [[ -n "${RUN_EMPTY_FETCH:-}" ]]; then
      # shellcheck disable=SC2317
      fetch_url() { printf ''; }
    fi
    if [[ -n "${RUN_DEAD_FETCH:-}" ]]; then
      # shellcheck disable=SC2317
      fetch_url() { return 1; }
    fi
    dep_pins_main
  ) > "$out_file" 2>&1
  rc=$?
  out=$(cat "$out_file")
  rm -f "$out_file"
}

# The canonical success line: both revisions, the offchain revision actually
# consulted, the project file read, and the URL the remote side came from.
canonical_re="^dep-pins: OK moog_onchain=$MOOG_ONCHAIN offchain_rev=$OFFCHAIN_A offchain_onchain=$MOOG_ONCHAIN project=cabal\.project source=https://raw\.githubusercontent\.com/lambdasistemi/cardano-mpfs-offchain/$OFFCHAIN_A/cabal\.project$"

# --- cases ------------------------------------------------------------------

t_parse_emits_stanzas() {
  moog_project "$MOOG_ONCHAIN"
  local got
  got=$(parse_srps "$fx/cabal.project")
  assert_eq "parse: exactly two stanzas" "2" "$(printf '%s\n' "$got" | grep -c .)"
  assert_match "parse: offchain stanza with tag and line" \
    "^https://github\.com/lambdasistemi/cardano-mpfs-offchain\.git$(printf '\t')$OFFCHAIN_A$(printf '\t')6$" \
    "$got"
  assert_match "parse: onchain stanza with tag and line" \
    "^https://github\.com/cardano-foundation/cardano-mpfs-onchain$(printf '\t')$MOOG_ONCHAIN$(printf '\t')11$" \
    "$got"
}

t_equal_fixtures_pass() {
  moog_project "$MOOG_ONCHAIN"
  run_check
  assert_eq "e2e fixture: agreeing pins exit 0" "0" "$rc"
  assert_match "e2e fixture: canonical OK line with full provenance" "$canonical_re" "$out"
  assert_eq "e2e fixture: exactly one canonical line" "1" "$(printf '%s\n' "$out" | grep -cE '^dep-pins: OK ')"
}

t_differing_fixtures_fail() {
  moog_project "$MOOG_ONCHAIN"
  run_check "DEP_PINS_OFFCHAIN_REV=$OFFCHAIN_B"
  assert_eq "e2e fixture: skewed pins exit 3" "3" "$rc"
  assert_match "mismatch names moog's revision" "$MOOG_ONCHAIN" "$out"
  assert_match "mismatch names the offchain-declared revision" "$DECLARED_BY_B" "$out"
  assert_match "mismatch names moog's file position" "moog cabal\.project:11" "$out"
  assert_match "mismatch names the offchain file position" "offchain $OFFCHAIN_B cabal\.project:4" "$out"
  assert_match "mismatch names the remote source" "read cardano-mpfs-onchain from https://raw\.githubusercontent\.com/.*$OFFCHAIN_B/cabal\.project" "$out"
  assert_absent "mismatch never passes" "^dep-pins: (OK|PASS-OVERRIDE) " "$out"
}

# F1 property: an ambient source override must never produce the canonical
# success token, and an overridden pass must name itself and its provenance.
t_override_agreeing_is_named() {
  moog_project "$MOOG_ONCHAIN"
  run_check "DEP_PINS_OFFCHAIN_REV=$OFFCHAIN_A"
  assert_eq "override agreeing: exits 0" "0" "$rc"
  assert_match "override agreeing: names itself PASS-OVERRIDE" "^dep-pins: PASS-OVERRIDE moog_onchain=$MOOG_ONCHAIN offchain_rev=$OFFCHAIN_A offchain_onchain=$MOOG_ONCHAIN .*override=DEP_PINS_OFFCHAIN_REV\$" "$out"
  assert_absent "override agreeing: never the canonical token" "^dep-pins: OK " "$out"
}

t_overridden_projectfile_never_says_ok() {
  moog_project "$MOOG_ONCHAIN"
  run_check "DEP_PINS_PROJECT_FILE=$fx/cabal.project"
  assert_eq "project override agreeing: exits 0" "0" "$rc"
  assert_match "project override: names itself PASS-OVERRIDE" "^dep-pins: PASS-OVERRIDE .*override=DEP_PINS_PROJECT_FILE\$" "$out"
  assert_absent "project override: never the canonical token" "^dep-pins: OK " "$out"
  assert_match "project override: prints the file actually read" "project=$fx/cabal.project" "$out"
}

t_missing_stanza_fails() {
  moog_project absent
  run_check
  assert_eq "missing onchain stanza exits 2" "2" "$rc"
  # Discriminate the guard's OWN diagnostic: the no-stanza message naming the
  # key and the file, not the exit status a downstream fallback produces.
  assert_match "missing stanza: own diagnostic" \
    "no source-repository-package stanza for cardano-mpfs-onchain in moog cabal\.project" "$out"
  assert_absent "missing stanza: not misread as a bad tag" "not a full 40-hex revision" "$out"
  assert_absent "missing stanza never passes" "^dep-pins: (OK|PASS-OVERRIDE) " "$out"
}

t_ambiguous_stanza_fails() {
  moog_project duplicated
  run_check
  assert_eq "ambiguous onchain stanzas exit 2" "2" "$rc"
  assert_match "ambiguity: own diagnostic" "ambiguous: 2 stanzas for cardano-mpfs-onchain in moog cabal\.project" "$out"
}

t_malformed_tag_fails() {
  moog_project short
  run_check
  assert_eq "non-40-hex tag exits 2" "2" "$rc"
  assert_match "malformed tag: own diagnostic" "not a full 40-hex revision" "$out"
  assert_match "malformed tag is named" "$SHORT_REV" "$out"
  assert_match "malformed tag names its file position" "cabal\.project:11" "$out"
}

t_unparseable_remote_fails() {
  moog_project "$MOOG_ONCHAIN"
  run_check "DEP_PINS_OFFCHAIN_REV=$OFFCHAIN_UNKNOWN"
  assert_eq "unparseable fetched body exits 2" "2" "$rc"
  assert_match "unparseable body: own diagnostic names key and source" \
    "no source-repository-package stanza for cardano-mpfs-onchain in https://raw\.githubusercontent\.com/.*$OFFCHAIN_UNKNOWN/cabal\.project" "$out"
  assert_absent "unparseable body never passes" "^dep-pins: (OK|PASS-OVERRIDE) " "$out"
}

t_empty_fetch_fails_closed() {
  moog_project "$MOOG_ONCHAIN"
  RUN_EMPTY_FETCH=1 run_check "DEP_PINS_OFFCHAIN_REV=$OFFCHAIN_A"
  assert_eq "empty fetched body exits 2" "2" "$rc"
  # Discriminate the empty-body guard: it must be THIS diagnostic, not the
  # no-stanza fallback an empty body would otherwise fall through to.
  assert_match "empty fetch: own diagnostic" "fetched empty content from https://raw\.githubusercontent\.com/.*$OFFCHAIN_A/cabal\.project" "$out"
  assert_absent "empty fetch: not misread as a dropped stanza" "no source-repository-package stanza" "$out"
  assert_absent "empty fetch never passes" "^dep-pins: (OK|PASS-OVERRIDE) " "$out"
}

t_dead_fetch_fails_closed() {
  moog_project "$MOOG_ONCHAIN"
  RUN_DEAD_FETCH=1 run_check "DEP_PINS_OFFCHAIN_REV=$OFFCHAIN_A"
  assert_eq "failed fetch exits 2" "2" "$rc"
  assert_match "failed fetch: own diagnostic" "could not fetch https://raw\.githubusercontent\.com/.*$OFFCHAIN_A/cabal\.project" "$out"
  assert_absent "failed fetch: not the empty-body diagnostic" "fetched empty content" "$out"
  assert_absent "failed fetch never passes" "^dep-pins: (OK|PASS-OVERRIDE) " "$out"
}

t_credential_free_by_construction() {
  # The transport must be plain curl with no auth material. This asserts the
  # source text rather than the runtime: the check must not read or send any
  # token (INV-101-PIN-NOCRED).
  local src
  src=$(cat "$here/dep-pins.sh")
  assert_absent "no token read from the environment" "MOOG_GITHUB_PAT|GITHUB_TOKEN|Authorization|GH_TOKEN" "$src"
  assert_match "transport is curl" "curl " "$src"
}

main_test() {
  t_parse_emits_stanzas
  t_equal_fixtures_pass
  t_differing_fixtures_fail
  t_override_agreeing_is_named
  t_overridden_projectfile_never_says_ok
  t_missing_stanza_fails
  t_ambiguous_stanza_fails
  t_malformed_tag_fails
  t_unparseable_remote_fails
  t_empty_fetch_fails_closed
  t_dead_fetch_fails_closed
  t_credential_free_by_construction
  printf 'dep-pins-test: pass=%d fail=%d\n' "$pass" "$fail" >&2
  ((fail == 0)) || return 1
}

main_test
