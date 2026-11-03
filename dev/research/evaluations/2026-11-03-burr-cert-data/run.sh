#!/bin/sh
set -eu
# Invoke this script under /tmp/umvl-cpu/run; do not nest slot runners.
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
mkdir -p "$TMPDIR" build/scratch
study=dev/research/evaluations/2026-11-03-burr-cert-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$study/probe.cpp" -o build/scratch/burr-cert-probe
python3 "$study/study.py"
