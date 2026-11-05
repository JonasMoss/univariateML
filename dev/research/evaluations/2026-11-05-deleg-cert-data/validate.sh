#!/usr/bin/env bash
# Run from the deleg-cert worktree root. Each job reserves CPU slots itself.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
mkdir -p "$TMPDIR" build/scratch
run_stage() {
  local stage="$1"
  shift
  /tmp/umvl-cpu/run4 nice -n 10 "$@" > "build/scratch/deleg-cert-${stage}.log" 2>&1
  tail -n 8 "build/scratch/deleg-cert-${stage}.log"
}
run_stage cpp-dev just cpp-test
run_stage cpp-no-long-double just cpp-test-no-long-double
run_stage r-dev just test
run_stage js-build just js-build
run_stage js-test just js-test
run_stage cpp-full env UMVL_TEST_TIER=full just cpp-test
run_stage r-full just test-full
