#!/usr/bin/env bash
# Run from the repository root through /tmp/umvl-cpu/run.
set -euo pipefail
evidence=dev/research/evaluations/2026-11-21-ew-limits-data
historical=dev/research/evaluations/2026-11-12-ew-fit-data
mkdir -p build/scratch
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
for mode in native double; do
    flags=()
    if [[ $mode == double ]]; then
        flags+=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS)
    fi
    ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "${flags[@]}" \
        "$evidence/replay.cpp" -o "build/scratch/ew-replay-$mode"
    "build/scratch/ew-replay-$mode" "$historical/archive-samples.csv" > "$evidence/archive-$mode.csv"
done
python3 "$evidence/replay-diff.py" > "$evidence/archive-diff.txt"
