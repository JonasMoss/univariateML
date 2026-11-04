#!/bin/sh
set -eu
# Invoke from the worktree root THROUGH /tmp/umvl-cpu/run. No nested locks.
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export CCACHE_DIR="$HOME/.cache/ccache/univariateML" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1
mkdir -p "$TMPDIR" build/scratch
study=dev/research/evaluations/2026-11-04-burr-port-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
ccache c++ -std=c++17 -O2 -Wall -Wextra -Wpedantic -IunivariateML-cpp/include -I"$boost" "$study/probe.cpp" -o build/scratch/burr-port-probe
c++ -std=c++17 -O2 -fno-exceptions -Wall -Wextra -Wpedantic -IunivariateML-cpp/include "$study/components.cpp" -o build/scratch/burr-port-components
c++ -std=c++17 -O2 -fno-exceptions -Wall -Wextra -Wpedantic -IunivariateML-cpp/include "$study/coordinates.cpp" -o build/scratch/burr-port-coordinates
python3 "$study/check.py"
