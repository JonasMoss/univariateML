#!/usr/bin/env bash
# Run from the worktree root; preserve the machine and full exit status.
set -u
name=$1
shift
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
rm -f "build/scratch/$name.result"
/tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=3 CTEST_PARALLEL_LEVEL=3 TESTTHAT_CPUS=3 "$@" > "build/scratch/$name.log" 2>&1
status=$?
machine=simbox
if [ "$status" -eq 75 ]; then
  machine=workstation-fallback
  /tmp/umvl-cpu/run4 "$@" >> "build/scratch/$name.log" 2>&1
  status=$?
fi
printf '%s,%s,%s\n' "$name" "$machine" "$status" > "build/scratch/$name.result"
tail -n 12 "build/scratch/$name.log"
exit "$status"
