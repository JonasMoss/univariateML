#!/usr/bin/env bash
# Invoke from the repository root through /tmp/umvl-cpu/run.
# R and WASM captures require `just regen` and `just js-build` first.
set -euo pipefail
evidence=dev/research/evaluations/2026-11-10-acc-allfree-data
mkdir -p build/scratch build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
# pkgload may compile the generated R package; this script reserves one slot.
export MAKEFLAGS=-j1
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
python3 "$evidence/capture.py" >build/scratch/capture.cpp
python3 "$evidence/replay.py" >build/scratch/replay.cpp
for mode in native double; do
  flags=()
  if test "$mode" = double; then flags=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); fi
  c++ -O2 -std=c++17 "${flags[@]}" -I. -IunivariateML-cpp/include -I"$boost" build/scratch/capture.cpp -o "build/scratch/capture-$mode"
  "build/scratch/capture-$mode" >"$evidence/after-$mode.jsonl"
done
c++ -O2 -std=c++17 -I. -IunivariateML-cpp/include -I"$boost" build/scratch/replay.cpp -o build/scratch/replay-after
build/scratch/replay-after >"$evidence/replay-after.jsonl"
Rscript "$evidence/capture.R" >"$evidence/after-r.jsonl"
node --v8-pool-size=1 "$evidence/capture.mjs" >"$evidence/after-wasm.jsonl"
python3 "$evidence/measure.py"
python3 "$evidence/analyze_replay.py"
