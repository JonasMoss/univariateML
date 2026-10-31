#!/usr/bin/env bash
# Invoke under /tmp/umvl-cpu/run[4], from the repository root. No nested locks.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
boost_include="${Boost_INCLUDE_DIR:-${BOOST_INCLUDE_DIR:-}}"
if [[ -z "$boost_include" && -f univariateML-cpp/build/CMakeCache.txt ]]; then
  boost_include=$(sed -n 's/^Boost_INCLUDE_DIR:[^=]*=//p' univariateML-cpp/build/CMakeCache.txt | head -1)
fi
# Monorepo convenience fallback supplies a plain include path, never R linkage.
if [[ -z "$boost_include" ]]; then
  boost_include=$(Rscript -e 'cat(system.file("include", package="BH"))')
fi
"${CXX:-c++}" -std=c++17 -O2 -Wall -Wextra -Wpedantic \
  -IunivariateML-cpp/include -I"$boost_include" \
  dev/validation/inference-4-probe.cpp -o build/scratch/inference-4-probe
python3 dev/validation/inference-4.py "$@"
