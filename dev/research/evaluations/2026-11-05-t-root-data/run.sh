#!/bin/sh
set -eu
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export PYTHONDONTWRITEBYTECODE=1
python3 dev/research/evaluations/2026-11-05-t-root-data/roots.py
python3 dev/research/evaluations/2026-11-05-t-root-data/global.py
python3 dev/research/evaluations/2026-11-05-t-root-data/verify.py
python3 dev/research/evaluations/2026-11-05-t-root-data/adversarial.py
python3 dev/research/evaluations/2026-11-05-t-root-data/shallow.py
boost_include=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_include" dev/research/evaluations/2026-11-04-t-cert-data/baseline.cpp -o build/scratch/t-root-baseline
python3 dev/research/evaluations/2026-11-05-t-root-data/baseline.py
