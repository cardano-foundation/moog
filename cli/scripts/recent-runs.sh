#!/usr/bin/env bash
set -euo pipefail

# usage: ./scripts/recent-runs.sh [whose] [limit]
WHOSE="${1:-cfhal}"
LIMIT="${2:-7}"

# time of slot 0 (assuming constant slot length)
# hard-coded to preprod
START_TS="2022-06-20T00:00:00Z"

moog facts test-runs --whose "$WHOSE" \
| jq -r --arg start "$START_TS" --argjson limit "$LIMIT" '
  # slot -> POSIX seconds
  def slot_to_time($slot):
    ($start | fromdateiso8601) + ($slot | tonumber);

  # ANSI
  def green: "\u001b[32m";
  def red:   "\u001b[31m";
  def faint: "\u001b[2m";
  def reset: "\u001b[0m";

  # Collapse phase/outcome to a standard status
  def status($phase; $outcome):
    if    $outcome == "success"                            then "succeeded"
    elif  $outcome == "failure"                            then "failed"
    elif  $phase  == "rejected"                            then "rejected"
    elif  $phase  == "finished" and $outcome == "unknown"
                                                           then "unknown outcome"
    elif  $phase  == "accepted"                            then "accepted"
    elif  ($phase == "pending" or $phase == "running")     then "pending"
    else  "\($phase);\($outcome // "null")"
    end;

  def color_for($status):
    if    $status == "succeeded" then green
    elif  ($status == "failed" or $status == "rejected") then red
    else "" end;

  sort_by(.slot)
  | reverse
  | .[:$limit]
  | .[]
  | (slot_to_time(.slot) | strflocaltime("%Y-%m-%d %H:%M:%S")) as $ts
  | .key.commitId[:7]   as $commit
  | .value.phase        as $phase
  | .value.outcome      as $outcome
  | .key.directory      as $dir
  | .value.url          as $url
  | status($phase; $outcome) as $status
  |
    (
      (color_for($status))
      + "\($ts) \($commit) \($status) \($dir)"
      + (if (color_for($status)) != "" then reset else "" end)
    )
    +
    (if $url != null and $url != "" then
       "\n" + faint + $url + reset
     else
       ""
     end)
'
