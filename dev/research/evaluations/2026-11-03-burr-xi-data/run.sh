#!/bin/sh
set -eu
# Call this script through /tmp/umvl-cpu/run. Do not nest slot runners.
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export CCACHE_DIR="$HOME/.cache/ccache/univariateML" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1
mkdir -p "$TMPDIR" build/scratch
study=dev/research/evaluations/2026-11-03-burr-xi-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" \
    dev/research/evaluations/2026-11-03-burr-cert-data/probe.cpp -o build/scratch/burr-xi-probe
python3 "$study/study.py"
python3 "$study/verify.py"
python3 "$study/replay.py"
