#!/bin/bash
set -eu
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-07-budget-align-data/slash-probe.cpp -o build/scratch/slash-probe
build/scratch/slash-probe
