#!/bin/bash
set -euo pipefail
mode=$1
phase=$2
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
flags=()
if test "$mode" = double; then flags+=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); fi
inc=.
if test "$phase" = before; then inc=build/baseline; fi
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1
ccache c++ -O2 -std=c++17 "${flags[@]}" -I"$inc" -I"$inc/univariateML-cpp/include" -I"$boost" dev/research/evaluations/2026-11-11-acc-fb-t-data/capture.cpp -o "build/scratch/$phase-$mode"
"build/scratch/$phase-$mode" > "dev/research/evaluations/2026-11-11-acc-fb-t-data/$phase-$mode.jsonl"

"build/scratch/$phase-$mode" dev/research/evaluations/2026-11-11-acc-fb-t-data/golden-inputs.txt > "dev/research/evaluations/2026-11-11-acc-fb-t-data/golden-$phase-$mode.jsonl"
