#!/usr/bin/env bash
# Invoke through /tmp/umvl-cpu/run on workstation; simbox-run on simbox.
set -euo pipefail
mode=${1:?suite name required}
machine=${2:?machine required}
evidence=dev/research/evaluations/2026-11-09-proof-audit-data
mkdir -p build/tmp build/scratch/proof-audit
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CMAKE_BUILD_PARALLEL_LEVEL=1 CTEST_PARALLEL_LEVEL=1 MAKEFLAGS=-j1
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 BLIS_NUM_THREADS=1
export R_PROFILE_USER="$PWD/$evidence/single-thread.R"
case "$mode" in
  cpp-dev) command=(just cpp-test);;
  cpp-full) command=(env UMVL_TEST_TIER=full just cpp-test);;
  cpp-double) command=(just cpp-test-no-long-double);;
  r-dev) command=(just test);;
  r-full) command=(just test-full);;
  *) exit 2;;
esac
log="build/scratch/proof-audit/$machine-$mode.log"
start=$(date -u +%Y-%m-%dT%H:%M:%SZ)
set +e
"${command[@]}" > "$log" 2>&1
rc=$?
set -e
finish=$(date -u +%Y-%m-%dT%H:%M:%SZ)
printf '%s\t%s\t%s\t%s\t%s\n' "$machine" "$mode" "$start" "$finish" "$rc" >> "$evidence/suites.tsv"
tail -25 "$log"
exit "$rc"
