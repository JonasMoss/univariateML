#!/usr/bin/env bash
# One slot; compile and run sequentially in both precision modes.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
D=dev/research/evaluations/2026-11-14-core5b-b-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
for mode in native double; do
  flags=(); [ "$mode" != double ] || flags+=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS)
  c++ -std=gnu++17 -O2 "${flags[@]}" -I. -IunivariateML-cpp/include -I"$boost" "$D/preflight.cpp" -o "build/scratch/preflight-$mode" > "$D/preflight-$mode.log" 2>&1
  "build/scratch/preflight-$mode" >> "$D/preflight-$mode.log" 2>&1
  tail -1 "$D/preflight-$mode.log"
done
