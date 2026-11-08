#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
data=dev/research/evaluations/2026-11-08-bbinom-accept-data
python3 "$data/build-timing.py"
python3 "$data/timing.py" > "$data/timing.json"
python3 "$data/check.py" > "$data/oracle.json"
git diff --check
