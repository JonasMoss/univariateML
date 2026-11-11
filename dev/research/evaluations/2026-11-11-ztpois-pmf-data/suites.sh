#!/usr/bin/env bash
# Run sequentially, from the worktree root. simbox exit 75 falls back locally.
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
run_suite() {
 local label=$1; shift
 set +e
 /tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=3 CTEST_PARALLEL_LEVEL=3 "$@" > "build/scratch/$label.log" 2>&1
 local rc=$?
 set -e
 if [[ $rc == 75 ]]; then
  echo "$label: simbox unavailable, falling back to workstation"
  /tmp/umvl-cpu/run4 nice -n 10 "$@" >> "build/scratch/$label.log" 2>&1
  echo "$label workstation PASS"
 else
  [[ $rc == 0 ]] || { tail -80 "build/scratch/$label.log"; return "$rc"; }
  echo "$label simbox PASS"
 fi
}
run_suite cpp-double-simbox just cpp-test-no-long-double
run_suite r-dev just test
run_suite r-full just test-full
