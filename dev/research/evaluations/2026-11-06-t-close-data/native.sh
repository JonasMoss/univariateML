#!/bin/sh
# Caller reserves one workstation slot, or invokes through simbox-run.
set -eu
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-06-t-close-data/replay.cpp -o build/scratch/t-close-final-replay
c++ -O2 -std=c++17 -IunivariateML-cpp/include dev/research/evaluations/2026-11-06-t-close-data/arithmetic.cpp -o build/scratch/t-close-arithmetic
python3 dev/research/evaluations/2026-11-06-t-close-data/replay.py
python3 dev/research/evaluations/2026-11-06-t-close-data/arithmetic.py
python3 dev/research/evaluations/2026-11-06-t-close-data/quantile-root.py
python3 dev/research/evaluations/2026-11-06-t-close-data/verify.py
