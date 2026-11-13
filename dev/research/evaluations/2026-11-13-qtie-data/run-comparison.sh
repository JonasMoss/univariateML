#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
root=$PWD/dev/research/evaluations/2026-11-13-qtie-data
boost=$(/tmp/umvl-cpu/run Rscript -e 'cat(system.file("include",package="BH"))')
flags=(-std=c++17 -O2 -I"$boost")
/tmp/umvl-cpu/run c++ "${flags[@]}" -IunivariateML-cpp/include "$root/compare.cpp" -o build/scratch/compare
/tmp/umvl-cpu/run build/scratch/compare > "$root/grid.csv"
/tmp/umvl-cpu/run Rscript "$root/stats.R" "$root/grid.csv" "$root/disagreements.csv" > "$root/stats-summary.txt"
/tmp/umvl-cpu/run python3 "$root/classify.py" > "$root/classification.txt"
