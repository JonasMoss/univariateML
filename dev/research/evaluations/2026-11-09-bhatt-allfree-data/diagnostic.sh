#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
g++ -O2 -std=c++17 -DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\" -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" dev/research/evaluations/2026-11-09-bhatt-allfree-data/diagnostic.cpp -o build/scratch/bhatt-diagnostic
build/scratch/bhatt-diagnostic
