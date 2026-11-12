#!/usr/bin/env bash
# Reconstruct baseline sources in ignored scratch; never keep source archives in evidence.
set -euo pipefail
baseline=3b2a16fed4787c71b96bf23b1cb86569973b0a9c
snapshot="$PWD/build/scratch/seam-check/baseline-$baseline"
mkdir -p "$snapshot" build/tmp
# git archive is read-only with respect to Git state.
git archive "$baseline" univariateML-cpp/include bindings/common dev/validation/quality \
  dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz | tar -x -C "$snapshot"
cp dev/validation/seam-check.py dev/validation/seam_reference.py "$snapshot/dev/validation/"
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run python3 "$snapshot/dev/validation/seam-check.py" \
  --output "$snapshot/build/scratch/baseline.jsonl" "$@"
