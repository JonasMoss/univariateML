#!/usr/bin/env bash
# Run from the worktree root through /tmp/umvl-cpu/run (one core).
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
boost_path=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests "-I$boost_path" \
  "-DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\"" \
  dev/research/evaluations/2026-11-08-betapr-joint-data/probe.cpp -o build/scratch/joint-probe
build/scratch/joint-probe > dev/research/evaluations/2026-11-08-betapr-joint-data/probe.txt
python3 dev/research/evaluations/2026-11-08-betapr-joint-data/check.py
