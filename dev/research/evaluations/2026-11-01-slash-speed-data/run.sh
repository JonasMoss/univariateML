#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
path=dev/research/evaluations/2026-11-01-slash-speed-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
/tmp/umvl-cpu/run c++ -std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" "$path/compare.cpp" -o build/scratch/slash-compare
/tmp/umvl-cpu/run build/scratch/slash-compare > "$path/paired.csv"
/tmp/umvl-cpu/run build/scratch/slash-compare golden > "$path/golden.csv"
python3 "$path/summarize.py"
python3 "$path/profile.py"
/tmp/umvl-cpu/run c++ -std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" build/scratch/slash-profile.cpp -o build/scratch/slash-profile
/tmp/umvl-cpu/run build/scratch/slash-profile > "$path/profile.csv"
