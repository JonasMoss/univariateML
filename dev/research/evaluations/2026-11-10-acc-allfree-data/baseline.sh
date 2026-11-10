#!/usr/bin/env bash
# Invoke from the repository root through /tmp/umvl-cpu/run.
# git archive/show only read the base; this script does not change Git state.
set -euo pipefail
baseline_revision=c60d92ea667e8dcb55ec6c6cbd1aaae474846d45
evidence=dev/research/evaluations/2026-11-10-acc-allfree-data
before=build/scratch/acc-allfree-baseline
mkdir -p "$before" build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
git archive "$baseline_revision" univariateML-cpp/include bindings/common | tar -x -C "$before"
python3 "$evidence/capture.py" --baseline >build/scratch/capture-before.cpp
python3 "$evidence/replay.py" >build/scratch/replay.cpp
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
for mode in native double; do
  flags=()
  if test "$mode" = double; then flags=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); fi
  c++ -O2 -std=c++17 "${flags[@]}" -I"$before" -I"$before/univariateML-cpp/include" -I"$boost" build/scratch/capture-before.cpp -o "build/scratch/capture-before-$mode"
  "build/scratch/capture-before-$mode" >"$evidence/before-$mode.jsonl"
done
c++ -O2 -std=c++17 -I"$before" -I"$before/univariateML-cpp/include" -I"$boost" build/scratch/replay.cpp -o build/scratch/replay-before
build/scratch/replay-before >"$evidence/replay-before.jsonl"
