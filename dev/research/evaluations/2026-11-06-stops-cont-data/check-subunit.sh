#!/bin/bash
set -euo pipefail
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
g++ -std=c++17 -O1 -I. -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-06-stops-cont-data/check-subunit.cpp -o build/scratch/check-subunit
build/scratch/check-subunit
