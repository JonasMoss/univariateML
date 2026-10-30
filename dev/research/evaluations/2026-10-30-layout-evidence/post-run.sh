#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp NOT_CRAN=true CCACHE_DIR=$HOME/.cache/ccache/univariateML
for target in cpp-test test quality regen; do
  echo "Starting $target $(date -u --iso-8601=seconds)"
  ccache --print-stats > "build/scratch/post/$target.cache-before.tsv"
  /usr/bin/time -f 'wall_seconds=%e' -o "build/scratch/post/$target.time" just "$target" > "build/scratch/post/$target.full.log" 2>&1
  ccache --print-stats > "build/scratch/post/$target.cache-after.tsv"
  echo "$target exit=0" >> build/scratch/post/exits.log
  tail -5 "build/scratch/post/$target.full.log"
  if [[ $target == cpp-test ]]; then cp univariateML-cpp/build/Testing/Temporary/LastTest.log build/scratch/post/cpp-targets.log; fi
  if [[ $target == quality ]]; then cp build/quality/report.md build/scratch/post/quality-families.log; cp build/quality/*.json build/scratch/post/; fi
done
ccache -s > build/scratch/post/ccache-stats.log
ccache --print-stats > build/scratch/post/ccache-stats.tsv
