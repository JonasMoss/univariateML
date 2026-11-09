#!/usr/bin/env bash
# Invoke through the workstation slot wrapper; computes only, with no nested lock.
set -euo pipefail
mode=${1:?check or boundaries}
case "$mode" in check|boundaries) ;; *) exit 2;; esac
evidence=dev/research/evaluations/2026-11-09-proof-audit-data
mkdir -p build/tmp build/scratch/proof-audit
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export PYTHONDONTWRITEBYTECODE=1 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
log=build/scratch/proof-audit/workstation-$mode.log
start=$(date -u +%Y-%m-%dT%H:%M:%SZ)
set +e
python3 "$evidence/$mode.py" > "$log" 2>&1
rc=$?
set -e
finish=$(date -u +%Y-%m-%dT%H:%M:%SZ)
printf '%s\t%s\t%s\t%s\t%s\n' workstation "$mode" "$start" "$finish" "$rc" >> "$evidence/computations.tsv"
tail -8 "$log"
exit "$rc"
