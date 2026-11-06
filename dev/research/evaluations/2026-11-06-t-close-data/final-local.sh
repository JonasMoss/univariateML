#!/bin/sh
set -eu
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
for suite in js-build js-test site-check relations; do
  echo "BEGIN $suite $(date -u +%FT%TZ)"
  /tmp/umvl-cpu/run4 just "$suite" > "build/scratch/t-close-$suite.log" 2>&1 || { tail -n 100 "build/scratch/t-close-$suite.log"; exit 1; }
  tail -n 12 "build/scratch/t-close-$suite.log"
  echo "END $suite $(date -u +%FT%TZ)"
done
