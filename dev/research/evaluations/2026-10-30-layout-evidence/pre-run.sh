#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp NOT_CRAN=true
export CCACHE_DIR=$HOME/.cache/ccache/univariateML
mkdir -p "$TMPDIR"
git rev-parse HEAD > build/scratch/pre/revision.log
{ date -u; uname -a; R --version; python3 --version; ccache --version; } > build/scratch/pre/environment.log
for target in cpp-test test quality regen; do
  echo "Starting $target $(date -u --iso-8601=seconds)"
  /usr/bin/time -f 'wall_seconds=%e' -o "build/scratch/pre/$target.time" just "$target" > "build/scratch/pre/$target.full.log" 2>&1
  echo "$target exit=0" >> build/scratch/pre/exits.log
  tail -5 "build/scratch/pre/$target.full.log"
  if [[ $target == cpp-test ]]; then cp cppunivariate/build/Testing/Temporary/LastTest.log build/scratch/pre/cpp-targets.log; fi
  if [[ $target == quality ]]; then cp build/quality/report.md build/scratch/pre/quality-families.log; cp build/quality/*.json build/scratch/pre/; fi
done
ccache -s > build/scratch/pre/ccache-stats.log
ccache --print-stats > build/scratch/pre/ccache-stats.tsv
cp -a univariateML/package build/scratch/pre-package
python3 build/scratch/pre/inventory.py
