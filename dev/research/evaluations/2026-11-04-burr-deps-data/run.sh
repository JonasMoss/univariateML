#!/bin/sh
set -eu
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
mkdir -p build/scratch
boost_include=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -I"$boost_include" -std=c++17 -O2 -IunivariateML-cpp/include dev/research/evaluations/2026-11-04-burr-deps-data/timing.cpp -o build/scratch/burr-deps-timing
build/scratch/burr-deps-timing < dev/research/evaluations/2026-11-04-burr-deps-data/inputs.txt > dev/research/evaluations/2026-11-04-burr-deps-data/timing.txt
