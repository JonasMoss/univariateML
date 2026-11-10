#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch/triang-baseline
/tmp/umvl-cpu/run bash -euo pipefail -c '
  evidence=dev/research/evaluations/2026-11-10-triang-endpoint-data
  baseline=build/scratch/triang-baseline
  cp -a univariateML-cpp/include/cppunivariate "$baseline/"
  git show cd145ca5:univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp > "$baseline/cppunivariate/distributions/triang/mle.hpp"
  boost=$(Rscript -e '\''cat(system.file("include",package="BH"))'\'')
  c++ -O1 -std=c++17 -I"$baseline" -I"$boost" "$evidence/replay.cpp" -o build/scratch/triang-before
  c++ -O1 -std=c++17 -IunivariateML-cpp/include -I"$boost" "$evidence/replay.cpp" -o build/scratch/triang-after
  for phase in before after; do
    python3 "$evidence/replay.py" "build/scratch/triang-$phase" > "$evidence/$phase.csv"
    CONTRACT=1 python3 "$evidence/replay.py" "build/scratch/triang-$phase" > "$evidence/$phase-contract.csv"
    EXHAUSTIVE=1 python3 "$evidence/replay.py" "build/scratch/triang-$phase" > "$evidence/$phase-exhaustive.csv"
    CONTRACT=1 EXHAUSTIVE=1 python3 "$evidence/replay.py" "build/scratch/triang-$phase" > "$evidence/$phase-contract-exhaustive.csv"
  done
  python3 "$evidence/reference.py"
  python3 "$evidence/validate.py"
'
