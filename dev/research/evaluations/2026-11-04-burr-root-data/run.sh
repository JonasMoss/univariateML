#!/bin/sh
set -eu
# Run via /tmp/umvl-cpu/run; never nest slot locks.
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export CCACHE_DIR="$HOME/.cache/ccache/univariateML" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1
mkdir -p "$TMPDIR" build/scratch
study=dev/research/evaluations/2026-11-04-burr-root-data
python3 "$study/isolate.py"
python3 "$study/global_check.py"
python3 "$study/exact_root.py"
python3 "$study/verify.py"
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" \
    "$study/benchmark.cpp" -o build/scratch/burr-root-benchmark
python3 "$study/benchmark.py"
python3 "$study/summarize.py"
