#!/usr/bin/env bash
# One reserved workstation slot for the entire sequence; no nested reservations.
set -euo pipefail
evidence=dev/research/evaluations/2026-11-09-proof-audit-data
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
python3 "$evidence/check.py"
python3 "$evidence/boundaries.py"
for mode in cpp-full cpp-double r-dev r-full; do
  bash "$evidence/suite.sh" "$mode" workstation
done
