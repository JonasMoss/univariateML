#!/bin/sh
# Invoke under one CPU slot, or through the supplied simbox launcher.
set -eu
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 PYTHONDONTWRITEBYTECODE=1
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-06-t-port-data/check-gamma.cpp -o build/scratch/t-port-gamma
python3 dev/research/evaluations/2026-11-06-t-port-data/check-gamma.py
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-06-t-port-data/replay.cpp -o build/scratch/t-port-replay
python3 dev/research/evaluations/2026-11-06-t-port-data/replay.py
python3 dev/research/evaluations/2026-11-06-t-port-data/verify.py
c++ -O2 -std=c++17 -IunivariateML-cpp/include dev/research/evaluations/2026-11-06-t-port-data/arithmetic.cpp -o build/scratch/t-port-arithmetic
python3 dev/research/evaluations/2026-11-06-t-port-data/arithmetic.py
python3 dev/research/evaluations/2026-11-06-t-port-data/adversarial.py
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-06-t-port-data/stress.cpp -o build/scratch/t-port-stress
build/scratch/t-port-stress > dev/research/evaluations/2026-11-06-t-port-data/stress.txt
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-06-t-port-data/shallow.cpp -o build/scratch/t-port-shallow
build/scratch/t-port-shallow > dev/research/evaluations/2026-11-06-t-port-data/shallow.txt
c++ -O2 -std=c++17 -IunivariateML-cpp/include dev/research/evaluations/2026-11-06-t-port-data/cutoff.cpp -o build/scratch/t-port-cutoff
python3 dev/research/evaluations/2026-11-06-t-port-data/cutoff.py
python3 dev/research/evaluations/2026-11-06-t-port-data/native-manifest.py
