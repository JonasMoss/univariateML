#!/usr/bin/env bash
# Run through run4 (EMCC_CORES=4); builds are sequential.
set -euo pipefail
cd "$(dirname "$0")/../.."
for opt in O2 O3 Os; do bash bindings/js/build.sh "$opt" "$opt"; done
for family in EXP GAMMA GB2; do bash bindings/js/build.sh O2 "without-${family}" "-DOMIT_${family}"; done
node --v8-pool-size=1 bindings/js/benchmark.mjs
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
c++ -std=c++17 -Wall -Wextra -Wpedantic -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" bindings/js/benchmark.cpp -o build/scratch/native-benchmark
build/scratch/native-benchmark > build/scratch/native-benchmark.txt
c++ -O2 -std=c++17 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" bindings/js/benchmark.cpp -o build/scratch/native-benchmark-O2
build/scratch/native-benchmark-O2 > build/scratch/native-benchmark-O2.txt
