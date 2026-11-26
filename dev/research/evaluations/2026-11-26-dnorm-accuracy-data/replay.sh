#!/usr/bin/env bash
# Run from the repository root. Outputs go in build/scratch.
set -euo pipefail
mode=${1:-native}
case "$mode" in
  master|native|double) ;;
  *) printf 'usage: %s [master|native|double]\n' "$0" >&2; exit 2 ;;
esac
scratch=$PWD/build/scratch
data=$PWD/dev/research/evaluations/2026-11-26-dnorm-accuracy-data
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export PYTHONDONTWRITEBYTECODE=1
mkdir -p "$scratch" "$TMPDIR"
slot=/tmp/umvl-cpu/run
include=$PWD/univariateML-cpp/include
flags=()
verify=()
case "$mode" in
  master)
    baseline=$(mktemp -d "$scratch/dnorm-master.XXXXXX")
    trap 'rm -rf "$baseline"' EXIT
    git archive 2d89fd4a univariateML-cpp/include | tar -x -C "$baseline"
    include=$baseline/univariateML-cpp/include
    ;;
  native) verify+=(verify) ;;
  double) flags+=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); verify+=(verify) ;;
esac
boost=$("$slot" Rscript -e 'cat(system.file("include", package="BH"))')
exe=$scratch/dnorm-replay-$mode
output=$exe.txt
"$slot" "${CXX:-c++}" -std=c++17 -O2 -I"$boost" -I"$include" "${flags[@]}" "$data/probe.cpp" -o "$exe"
"$slot" "$exe" | tee "$output"
"$slot" python3 -u "$data/reference.py" "$output" "${verify[@]}"
