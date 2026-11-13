#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
root=$PWD/dev/research/evaluations/2026-11-13-qtie-data
boost=$(/tmp/umvl-cpu/run Rscript -e 'cat(system.file("include",package="BH"))')
for mode in before after; do
 include=univariateML-cpp/include
 if [[ $mode == before ]]; then include=build/scratch/baseline/univariateML-cpp/include; fi
 /tmp/umvl-cpu/run c++ -std=c++17 -O2 -I"$include" -I"$boost" "$root/cdf-probe.cpp" -o "build/scratch/cdf-probe-$mode"
 /tmp/umvl-cpu/run "build/scratch/cdf-probe-$mode" > "$root/cdf-probe-$mode.csv"
done
