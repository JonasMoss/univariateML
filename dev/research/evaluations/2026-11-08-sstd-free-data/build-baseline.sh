#!/bin/bash
# Read-only Git access; reconstruct the one modified production header.
# Invoke through /tmp/umvl-cpu/run; no slot acquisition is nested here.
set -euo pipefail
D=dev/research/evaluations/2026-11-08-sstd-free-data
mkdir -p build/scratch/baseline-include
cp -a univariateML-cpp/include/cppunivariate build/scratch/baseline-include/
git show 09ab21746631b00f56aed243e16f52066f461be2:univariateML-cpp/include/cppunivariate/distributions/sstd/mle.hpp > build/scratch/baseline-include/cppunivariate/distributions/sstd/mle.hpp
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -I"$boost" -Ibuild/scratch/baseline-include -IunivariateML-cpp/tests "$D/timing.cpp" -o build/scratch/timing-before
