#!/bin/bash
# Invoke through /tmp/umvl-cpu/run from the worktree root.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
out=dev/research/evaluations/2026-11-09-gev-perf-data
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
stages=("$@"); if [[ ${#stages[@]} == 0 ]];then stages=(baseline final);fi
for stage in "${stages[@]}"; do
  for mode in plain instrument; do
    args=(); suffix=''
    if [[ "$mode" == instrument ]]; then args=(instrument); suffix=-profile; fi
    path=$(python3 "$out/profile.py" "$stage" "${args[@]}")
    flags=(); if [[ "$mode" == instrument ]]; then flags=(-include "$path/instrument.hpp"); fi
    c++ -std=c++17 -O2 -I"$path" -IunivariateML-cpp/include -I"$boost" -Ibuild/scratch "${flags[@]}" "$path/benchmark.cpp" -o "$path/benchmark"
    "$path/benchmark" > "$out/$stage$suffix.jsonl" 2> "$out/$stage$suffix.txt"
    if [[ "$mode" == plain ]]; then
      c++ -std=c++17 -O2 -I"$path" -IunivariateML-cpp/include -I"$boost" "$path/seeded.cpp" -o "$path/seeded"
      "$path/seeded" > "$out/$stage-seeded.txt"
    fi
  done
done
