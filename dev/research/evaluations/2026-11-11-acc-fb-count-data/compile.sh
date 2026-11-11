#!/usr/bin/env bash
set -euo pipefail
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
inc=()
if [[ "$*" == *before-* ]]; then inc=(-Ibuild/scratch/baseline -Ibuild/scratch/baseline/univariateML-cpp/include); fi
c++ "${inc[@]}" -std=c++17 -O2 -I. -IunivariateML-cpp/include -I"$boost" "$@" dev/research/evaluations/2026-11-11-acc-fb-count-data/study.cpp
