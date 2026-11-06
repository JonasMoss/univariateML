#!/bin/sh
set -eu
mkdir -p build/scratch/before/cppunivariate/distributions/lst
# Read the task's exact baseline without modifying the Git index or worktree.
git show f30584cd:univariateML-cpp/include/cppunivariate/distributions/lst/certificate.hpp > build/scratch/before/cppunivariate/distributions/lst/certificate.hpp
ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/core" build/scratch/before/cppunivariate/core
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -Ibuild/scratch/before -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-06-t-close-data/replay.cpp -o build/scratch/t-close-before
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-06-t-close-data/replay.cpp -o build/scratch/t-close-replay
python3 dev/research/evaluations/2026-11-06-t-close-data/benchmark.py
