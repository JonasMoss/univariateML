#!/usr/bin/env bash
# Run directly from this worktree; this script acquires its own CPU slots.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
remote_or_local() {
  local rc=0
  /tmp/umvl-cpu/simbox-run "$@" || rc=$?
  if [[ "$rc" == 75 ]]; then
    /tmp/umvl-cpu/run4 "$@"
  elif [[ "$rc" != 0 ]]; then
    return "$rc"
  fi
}
/tmp/umvl-cpu/run4 just cpp-test
remote_or_local env UMVL_TEST_TIER=full CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 just cpp-test
/tmp/umvl-cpu/run4 just cpp-test-no-long-double
remote_or_local env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 just cpp-test-no-long-double
remote_or_local env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 just test
remote_or_local env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 just test-full
/tmp/umvl-cpu/run env EMCC_CORES=1 just js-build
/tmp/umvl-cpu/run just js-test
/tmp/umvl-cpu/run env EMCC_CORES=1 just site-check
