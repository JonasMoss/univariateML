#!/usr/bin/env bash
# Run from repository root; no installs, no writes outside this worktree.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
sn_data=dev/research/evaluations/2026-11-01-sn-speed-data
sn_flags=(-std=c++17 -O2 -IunivariateML-cpp/include)
if ! test -d /usr/include/boost; then
  sn_boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
  sn_flags+=(-I"$sn_boost")
fi
/tmp/umvl-cpu/run c++ "${sn_flags[@]}" "$sn_data/compare.cpp" -o build/scratch/sn-compare
/tmp/umvl-cpu/run build/scratch/sn-compare golden > "$sn_data/golden.csv"
/tmp/umvl-cpu/run build/scratch/sn-compare golden legacy > "$sn_data/golden-legacy.csv"
/tmp/umvl-cpu/run build/scratch/sn-compare > "$sn_data/paired.csv"
/tmp/umvl-cpu/run python3 "$sn_data/summarize.py" > "$sn_data/summary.txt"
python3 "$sn_data/profile.py"
/tmp/umvl-cpu/run c++ "${sn_flags[@]}" -Ibuild/scratch "$sn_data/profile.cpp" -o build/scratch/sn-profile
/tmp/umvl-cpu/run build/scratch/sn-profile > "$sn_data/profile.csv" 2> "$sn_data/profile-points.csv"
