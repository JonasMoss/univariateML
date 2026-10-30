#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp NOT_CRAN=true CCACHE_DIR=$HOME/.cache/ccache/univariateML
for target in cpp-test regen; do
  echo "Starting unchanged $target $(date -u --iso-8601=seconds)"
  ccache -s > "build/scratch/rebuild/$target.cache-before.log"
  ccache --print-stats > "build/scratch/rebuild/$target.cache-before.tsv"
  /usr/bin/time -f 'wall_seconds=%e' -o "build/scratch/rebuild/$target.time" just "$target" > "build/scratch/rebuild/$target.full.log" 2>&1
  ccache -s > "build/scratch/rebuild/$target.cache-after.log"
  ccache --print-stats > "build/scratch/rebuild/$target.cache-after.tsv"
  tail -4 "build/scratch/rebuild/$target.full.log"
done
