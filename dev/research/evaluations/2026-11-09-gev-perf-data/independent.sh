#!/bin/bash
# Invoke through /tmp/umvl-cpu/run from the worktree root.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
out=dev/research/evaluations/2026-11-09-gev-perf-data
python3 "$out/profile.py" final > /dev/null
c++ -std=c++17 -O2 -IunivariateML-cpp/include -Ibuild/scratch dev/research/evaluations/2026-11-08-gev-allfree-data/joint-check.cpp -o build/scratch/perf-check
build/scratch/perf-check > "$out/joint-bounds.json"
UMVL_JOINT_OUTPUT="$out" python3 dev/research/evaluations/2026-11-08-gev-allfree-data/joint-check.py > "$out/joint-independent.json"
c++ -std=c++17 -O2 -IunivariateML-cpp/include "$out/corner-probe.cpp" -o build/scratch/corner-probe
build/scratch/corner-probe > "$out/corner-bounds.json"
python3 "$out/corner-check.py" > "$out/corner-independent.json"
