#!/bin/sh
set -eu
boost_include=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -I"$boost_include" -std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests dev/research/evaluations/2026-11-04-burr-deps-data/roots.cpp -o build/scratch/burr-deps-roots
build/scratch/burr-deps-roots > dev/research/evaluations/2026-11-04-burr-deps-data/roots.txt
python3 dev/research/evaluations/2026-11-04-burr-deps-data/verify.py
