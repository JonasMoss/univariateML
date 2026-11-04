#!/bin/sh
set -eu
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export PYTHONDONTWRITEBYTECODE=1
python3 dev/research/evaluations/2026-11-04-t-cert-data/study.py
boost_include=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_include" dev/research/evaluations/2026-11-04-t-cert-data/baseline.cpp -o build/scratch/t-cert-baseline
python3 dev/research/evaluations/2026-11-04-t-cert-data/baseline.py
python3 dev/research/evaluations/2026-11-04-t-cert-data/independent.py
python3 dev/research/evaluations/2026-11-04-t-cert-data/signed-tail.py
python3 dev/research/evaluations/2026-11-04-t-cert-data/exact.py
python3 dev/research/evaluations/2026-11-04-t-cert-data/local-tail.py
