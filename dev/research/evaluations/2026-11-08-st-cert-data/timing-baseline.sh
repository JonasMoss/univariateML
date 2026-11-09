#!/usr/bin/env bash
# Reproduce the baseline from immutable source without changing Git state.
# Invoke from the worktree root through /tmp/umvl-cpu/run (one CPU).
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR=/home/jonas/.cache/ccache/univariateML CCACHE_BASEDIR=$PWD CCACHE_NOHASHDIR=1
baseline=ddb399172bd99091c8d1b04dbf7fe329c8adc43e
baseline_dir=$PWD/build/scratch/st-baseline
out=dev/research/evaluations/2026-11-08-st-cert-data
mkdir -p "$TMPDIR" "$baseline_dir"
git archive "$baseline" univariateML-cpp/include | tar -xf - -C "$baseline_dir"
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
ccache c++ -std=c++17 -O2 -I"$baseline_dir/univariateML-cpp/include" -I"$boost" "$out/timing.cpp" -o build/scratch/st-before
build/scratch/st-before > "$out/timing-before.txt"
ccache c++ -std=c++17 -O2 -I"$baseline_dir/univariateML-cpp/include" -I"$boost" "$out/extra-timing.cpp" -o build/scratch/st-extra-before
build/scratch/st-extra-before > "$out/extra-timing-before.txt"
