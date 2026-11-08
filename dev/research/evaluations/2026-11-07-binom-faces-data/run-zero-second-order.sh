#!/bin/bash
set -euo pipefail
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-07-binom-faces-data/zero-second-order.cpp -o build/scratch/zero-second-order
build/scratch/zero-second-order
