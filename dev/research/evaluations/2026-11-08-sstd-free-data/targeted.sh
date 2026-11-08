#!/bin/bash
set -euo pipefail
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
flags=(-std=c++17 -O2 -I"$boost" -IunivariateML-cpp/include -IunivariateML-cpp/tests)
c++ "${flags[@]}" dev/research/evaluations/2026-11-08-sstd-free-data/probe.cpp -o build/scratch/probe
build/scratch/probe > dev/research/evaluations/2026-11-08-sstd-free-data/probe.txt
c++ "${flags[@]}" dev/research/evaluations/2026-11-08-sstd-free-data/bounds.cpp -o build/scratch/bounds
build/scratch/bounds > dev/research/evaluations/2026-11-08-sstd-free-data/bounds.txt
c++ "${flags[@]}" '-DCPPUNIV_DISTRIBUTIONS_DIR="univariateML-cpp/include/cppunivariate/distributions"' univariateML-cpp/tests/distribution_tests_main.cpp univariateML-cpp/include/cppunivariate/distributions/sstd/test.cpp -o build/scratch/test-sstd
build/scratch/test-sstd
