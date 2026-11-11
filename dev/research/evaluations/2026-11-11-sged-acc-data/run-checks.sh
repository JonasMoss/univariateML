#!/bin/bash
# Run from the worktree. Never wraps slot waits inside another reservation.
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
D=dev/research/evaluations/2026-11-11-sged-acc-data
mkdir -p "$TMPDIR" build/scratch
for name in "$@"; do
 case "$name" in
  targeted-final) cmd=(bash dev/research/evaluations/2026-11-11-sged-acc-data/targeted-final.sh);machine=simbox;;
  cpp-dev) cmd=(just cpp-test);machine=simbox;;
  cpp-full) cmd=(env UMVL_TEST_TIER=full just cpp-test);machine=simbox;;
  cpp-double-simbox) cmd=(just cpp-test-no-long-double);machine=simbox;;
  r-dev) cmd=(just test);machine=simbox;;
  r-full) cmd=(just test-full);machine=simbox;;
  cpp-double-local) cmd=(just cpp-test-no-long-double);machine=workstation;;
  site) cmd=(just site-check);machine=workstation;;
  *) exit 2;;
 esac
 start=$(date +%s)
 echo "START $name on $machine"
 rc=0
 if [ "$machine" = simbox ];then
  /tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 "${cmd[@]}" > "build/scratch/$name.log" 2>&1 || rc=$?
  if [ "$rc" = 75 ];then
   machine=workstation-fallback;rc=0
   /tmp/umvl-cpu/run4 "${cmd[@]}" >> "build/scratch/$name.log" 2>&1 || rc=$?
  fi
 else
  /tmp/umvl-cpu/run4 "${cmd[@]}" > "build/scratch/$name.log" 2>&1 || rc=$?
 fi
 elapsed=$(( $(date +%s)-start ))
 printf '%s %s rc=%s elapsed=%ss\n' "$name" "$machine" "$rc" "$elapsed" | tee -a "$D/test-runs.txt"
 tail -n 8 "build/scratch/$name.log"
 if [ "$rc" != 0 ];then exit "$rc";fi
done
