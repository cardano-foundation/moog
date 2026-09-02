#!/usr/bin/env bash
# shellcheck shell=bash
# dep-pins — C-DEP-PINS alignment check (M2-T101 slice SD1).
#
# Compares the cardano-mpfs-onchain revision moog declares in cabal.project
# with the revision declared by the cardano-mpfs-offchain revision moog pins,
# reading both from their real sources — never from a value transcribed into
# moog. The offchain repository is public: this check acquires no credential.
#
# Exit codes:
#   0  the two declared revisions agree; with no override in effect, stdout
#      carries the canonical line
#      `dep-pins: OK moog_onchain=<40hex> offchain_rev=<40hex> offchain_onchain=<40hex> project=<path> source=<https url>`
#   2  a source could not be read, fetched, or parsed (fail-closed — an
#      unreadable source must never degrade into a pass)
#   3  the two declared revisions differ; the output names both revisions and
#      both file positions
#
# Environment inputs the check honours — BOTH are overrides, and while either
# is set the canonical success token `dep-pins: OK` is never emitted; an
# agreeing run under an override prints `dep-pins: PASS-OVERRIDE …` naming the
# override instead, so a green under a redirected source can never be mistaken
# for a green about moog's declared sources:
#   DEP_PINS_PROJECT_FILE  replaces the project file read as moog's side
#                          (default `cabal.project` in the working directory)
#   DEP_PINS_OFFCHAIN_REV  replaces the offchain revision read from that
#                          project file (the gate's negative control uses this
#                          to point the check at a revision known to declare a
#                          different onchain pin)
#
# This file is sourced by scripts/dep-pins-test.sh, which drives the decision
# logic over fixtures without the network.

ONCHAIN_KEY="cardano-mpfs-onchain"
OFFCHAIN_KEY="cardano-mpfs-offchain"

# --- decision logic (network-free) ------------------------------------------

# parse_srps <file>
#   Print one line per source-repository-package stanza of <file>:
#     location<TAB>tag<TAB>tag-line-number
#   A stanza starts at a line beginning with `source-repository-package` and
#   ends at the next such line, the first column-0 non-comment line, or EOF.
parse_srps() {
  local file=$1
  if [[ ! -r "$file" ]]; then
    printf 'dep-pins: FAIL cannot read %s\n' "$file" >&2
    return 2
  fi
  local line lineno=0 in_stanza=0 loc="" tag="" tagline=0
  while IFS= read -r line || [[ -n "$line" ]]; do
    lineno=$((lineno + 1))
    if [[ "$line" == source-repository-package* ]]; then
      if ((in_stanza)); then printf '%s\t%s\t%s\n' "$loc" "$tag" "$tagline"; fi
      in_stanza=1
      loc=""
      tag=""
      tagline=0
      continue
    fi
    if ((in_stanza)) && [[ "$line" =~ ^[^[:space:]] ]]; then
      printf '%s\t%s\t%s\n' "$loc" "$tag" "$tagline"
      in_stanza=0
      loc=""
      tag=""
      tagline=0
      continue
    fi
    if ((in_stanza)); then
      if [[ "$line" =~ ^[[:space:]]*location:[[:space:]]*([^[:space:]#]+) ]]; then
        loc=${BASH_REMATCH[1]}
      elif [[ "$line" =~ ^[[:space:]]*tag:[[:space:]]*([^[:space:]#]+) ]]; then
        tag=${BASH_REMATCH[1]}
        tagline=$lineno
      fi
    fi
  done < "$file"
  if ((in_stanza)); then printf '%s\t%s\t%s\n' "$loc" "$tag" "$tagline"; fi
}

# require_srp <file> <location-substring> <label>
#   Fail-closed lookup of the single stanza whose location contains the key.
#   Returns 2 (with a message naming what could not be read) when the stanza
#   is missing, ambiguous, or its tag is not a full 40-hex revision. Prints
#   location<TAB>tag<TAB>tag-line-number on success.
require_srp() {
  local file=$1 key=$2 label=$3
  local parsed row keep="" n=0
  parsed=$(parse_srps "$file") || return 2
  while IFS= read -r row; do
    [[ -z "$row" ]] && continue
    if [[ "${row%%$'\t'*}" == *"$key"* ]]; then
      keep=$row
      n=$((n + 1))
    fi
  done <<< "$parsed"
  if ((n == 0)); then
    printf 'dep-pins: FAIL no source-repository-package stanza for %s in %s\n' "$key" "$label" >&2
    return 2
  fi
  if ((n > 1)); then
    printf 'dep-pins: FAIL ambiguous: %d stanzas for %s in %s\n' "$n" "$key" "$label" >&2
    return 2
  fi
  local loc=${keep%%$'\t'*}
  local rest=${keep#*$'\t'}
  local tag=${rest%%$'\t'*}
  local tagline=${rest##*$'\t'}
  if ! [[ "$tag" =~ ^[0-9a-f]{40}$ ]]; then
    printf 'dep-pins: FAIL %s declares %s tag %s (cabal.project:%s), which is not a full 40-hex revision\n' \
      "$label" "$key" "$tag" "$tagline" >&2
    return 2
  fi
  printf '%s\t%s\t%s\n' "$loc" "$tag" "$tagline"
}

# srp_field <1|2|3> <location<TAB>tag<TAB>line>
srp_field() {
  local rest
  case $1 in
    1) printf '%s\n' "${2%%$'\t'*}" ;;
    2)
      rest=${2#*$'\t'}
      printf '%s\n' "${rest%%$'\t'*}"
      ;;
    3) printf '%s\n' "${2##*$'\t'}" ;;
  esac
}

# raw_cabal_url <github-location> <revision>
#   Derive the raw cabal.project URL for a github repository location at a
#   revision. Fails when the location shape is not recognised.
raw_cabal_url() {
  local loc=${1%.git} rev=$2
  if [[ "$loc" =~ ^https://github\.com/(.+)$ || "$loc" =~ ^git@github\.com:(.+)$ ]]; then
    printf 'https://raw.githubusercontent.com/%s/%s/cabal.project\n' "${BASH_REMATCH[1]}" "$rev"
    return 0
  fi
  return 1
}

# --- wire (real sources) ----------------------------------------------------

# fetch_url <url>
#   Transport only: print the response body, non-zero on transport failure.
#   Plain curl, no credential material — the repositories involved are public.
fetch_url() {
  local url=$1 body
  if ! body=$(curl -fsSL --retry 3 --retry-delay 2 --max-time 60 "$url"); then
    return 1
  fi
  printf '%s' "$body"
}

# fetch_remote <url>
#   fetch_url plus the fail-closed content guards: a transport failure and an
#   empty body are distinct failures naming what could not be read.
fetch_remote() {
  local url=$1 body
  if ! body=$(fetch_url "$url"); then
    printf 'dep-pins: FAIL could not fetch %s\n' "$url" >&2
    return 2
  fi
  if [[ -z "$body" ]]; then
    printf 'dep-pins: FAIL fetched empty content from %s\n' "$url" >&2
    return 2
  fi
  printf '%s\n' "$body"
}

# dep_pins_main
#   The check itself. See the exit-code contract in the header.
dep_pins_main() {
  local project=${DEP_PINS_PROJECT_FILE:-cabal.project}
  local override=""
  local rec
  local moog_rev moog_line off_loc off_pin_rev
  local off_rev off_url tmp declared_rec declared_rev declared_line

  [[ -n "${DEP_PINS_PROJECT_FILE:-}" ]] && override="DEP_PINS_PROJECT_FILE"

  rec=$(require_srp "$project" "$ONCHAIN_KEY" "moog cabal.project") || return 2
  moog_rev=$(srp_field 2 "$rec")
  moog_line=$(srp_field 3 "$rec")

  rec=$(require_srp "$project" "$OFFCHAIN_KEY" "moog cabal.project") || return 2
  off_loc=$(srp_field 1 "$rec")
  off_pin_rev=$(srp_field 2 "$rec")

  if [[ -n "${DEP_PINS_OFFCHAIN_REV:-}" ]]; then
    off_rev=$DEP_PINS_OFFCHAIN_REV
    override="${override:+${override},}DEP_PINS_OFFCHAIN_REV"
    if ! [[ "$off_rev" =~ ^[0-9a-f]{40}$ ]]; then
      printf 'dep-pins: FAIL DEP_PINS_OFFCHAIN_REV %s is not a full 40-hex revision\n' "$off_rev" >&2
      return 2
    fi
  else
    off_rev=$off_pin_rev
  fi

  if ! off_url=$(raw_cabal_url "$off_loc" "$off_rev"); then
    printf 'dep-pins: FAIL cannot derive a raw cabal.project URL for %s at %s\n' "$off_loc" "$off_rev" >&2
    return 2
  fi

  tmp=$(mktemp "${TMPDIR:-/tmp}/dep-pins.XXXXXX")
  if ! fetch_remote "$off_url" > "$tmp"; then
    rm -f "$tmp"
    return 2
  fi
  declared_rec=$(require_srp "$tmp" "$ONCHAIN_KEY" "$off_url") || {
    rm -f "$tmp"
    return 2
  }
  rm -f "$tmp"
  declared_rev=$(srp_field 2 "$declared_rec")
  declared_line=$(srp_field 3 "$declared_rec")

  if [[ "$moog_rev" == "$declared_rev" ]]; then
    if [[ -z "$override" ]]; then
      printf 'dep-pins: OK moog_onchain=%s offchain_rev=%s offchain_onchain=%s project=%s source=%s\n' \
        "$moog_rev" "$off_rev" "$declared_rev" "$project" "$off_url"
    else
      # An override is in effect: this pass says so, and never wears the
      # canonical token that means "moog's declared sources agree".
      printf 'dep-pins: PASS-OVERRIDE moog_onchain=%s offchain_rev=%s offchain_onchain=%s project=%s source=%s override=%s\n' \
        "$moog_rev" "$off_rev" "$declared_rev" "$project" "$off_url" "$override"
    fi
    return 0
  fi

  printf 'dep-pins: MISMATCH moog cabal.project:%s declares %s %s\n' "$moog_line" "$ONCHAIN_KEY" "$moog_rev" >&2
  printf 'dep-pins: MISMATCH offchain %s cabal.project:%s declares %s %s\n' \
    "$off_rev" "$declared_line" "$ONCHAIN_KEY" "$declared_rev" >&2
  printf 'dep-pins: FAIL moog pins %s %s, whose own cabal.project declares %s %s — the composed build uses the moog pin and fails on the skew; move both pins together (docs/ops/mpfs-v2-canary.md); read %s from %s with project file %s\n' \
    "$OFFCHAIN_KEY" "$off_rev" "$ONCHAIN_KEY" "$declared_rev" "$ONCHAIN_KEY" "$off_url" "$project" >&2
  return 3
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  dep_pins_main "$@"
fi
