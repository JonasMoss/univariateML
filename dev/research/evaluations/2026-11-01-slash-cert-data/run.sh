#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
path=dev/research/evaluations/2026-11-01-slash-cert-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
/tmp/umvl-cpu/run c++ -std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" "$path/compare.cpp" -o build/scratch/slash-cert-compare
/tmp/umvl-cpu/run build/scratch/slash-cert-compare > "$path/paired.csv"
/tmp/umvl-cpu/run build/scratch/slash-cert-compare golden > "$path/golden.csv"
/tmp/umvl-cpu/run python3 "$path/summarize.py"
