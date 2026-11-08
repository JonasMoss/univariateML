#!/usr/bin/env bash
# Run from the worktree root. No repository writes or installs.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
rm -rf build/scratch/cmb-before
mkdir -p build/scratch/cmb-before
# All unchanged dependencies are symlinks to this worktree; only mle.hpp differs.
cp -as "$PWD/univariateML-cpp/include/." build/scratch/cmb-before/
rm build/scratch/cmb-before/cppunivariate/distributions/cmb/mle.hpp
git show 09ab2174:univariateML-cpp/include/cppunivariate/distributions/cmb/mle.hpp > build/scratch/cmb-before/cppunivariate/distributions/cmb/mle.hpp
boost=$(/tmp/umvl-cpu/run Rscript -e 'cat(system.file("include",package="BH"))')
data=dev/research/evaluations/2026-11-08-cmb-accept-data
/tmp/umvl-cpu/run c++ -O2 -std=c++17 -I"$boost" -Ibuild/scratch/cmb-before -IunivariateML-cpp/tests "$data/probe.cpp" -o build/scratch/cmb-before-probe
/tmp/umvl-cpu/run build/scratch/cmb-before-probe > "$data/before.txt"
/tmp/umvl-cpu/run c++ -O2 -std=c++17 -I"$boost" -IunivariateML-cpp/include -IunivariateML-cpp/tests "$data/probe.cpp" -o build/scratch/cmb-after-probe
/tmp/umvl-cpu/run build/scratch/cmb-after-probe > "$data/after.txt"
/tmp/umvl-cpu/run python3 "$data/accuracy.py"
