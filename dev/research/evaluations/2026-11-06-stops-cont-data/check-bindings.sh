#!/bin/bash
set -euo pipefail
ulimit -c 0
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
g++ -std=c++17 -O1 -I. -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-06-stops-cont-data/check-nomle.cpp -o build/scratch/stops-cont-binding-check
build/scratch/stops-cont-binding-check
g++ -std=c++17 -O1 -I. -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-06-stops-cont-data/check-ties.cpp -o build/scratch/check-ties-check
build/scratch/check-ties-check
