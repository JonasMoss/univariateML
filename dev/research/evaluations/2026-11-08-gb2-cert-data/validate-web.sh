#!/bin/bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
mkdir -p build/tmp build/scratch
for command in js-build js-test site-check; do
 if just "$command" > "build/scratch/gb2-$command.log" 2>&1; then
  printf '%s PASS\n' "$command"
  tail -8 "build/scratch/gb2-$command.log"
 else
  rc=$?
  tail -80 "build/scratch/gb2-$command.log"
  exit "$rc"
 fi
done
