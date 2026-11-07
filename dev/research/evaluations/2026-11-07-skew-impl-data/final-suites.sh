#!/usr/bin/env bash
# Run from the worktree root. No outer CPU lock: simbox-run owns its job
# slot, and the exit-75 fallback reserves all four local CPU slots itself.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
log=dev/research/evaluations/2026-11-07-skew-impl-data/remote-final.txt
commands='export XDG_RUNTIME_DIR="$TMPDIR" CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4; set -ex; hostname; just cpp-test; just test; UMVL_TEST_TIER=full just cpp-test; just cpp-test-no-long-double; just test-full'
rc=0
/tmp/umvl-cpu/simbox-run bash -c "$commands" > "$log" 2>&1 || rc=$?
if [ "$rc" -eq 75 ]; then
  /tmp/umvl-cpu/run4 bash -c "$commands" >> "$log" 2>&1
else
  exit "$rc"
fi
