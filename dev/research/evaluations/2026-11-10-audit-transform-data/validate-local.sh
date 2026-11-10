#!/usr/bin/env bash
# Invoke this script via run4; never nest reservations.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export GOMAXPROCS=2 UV_THREADPOOL_SIZE=1
mkdir -p "$TMPDIR" build/scratch
run_check() {
  local name="$1"; shift
  local rc=0
  "$@" > "build/scratch/$name.log" 2>&1 || rc=$?
  tail -n 16 "build/scratch/$name.log"
  echo "$name exit $rc"
  test "$rc" -eq 0
}
run_check native-clean cmake --build univariateML-cpp/build --target clean
export CCACHE_DISABLE=1
run_check r-dev-final just test
run_check cpp-dev-final just cpp-test
run_check cpp-full env UMVL_TEST_TIER=full just cpp-test
run_check cpp-no-long-double just cpp-test-no-long-double
run_check r-full just test-full
run_check js-build just js-build
run_check js-test just js-test
run_check site-check just site-check
