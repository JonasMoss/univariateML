#!/bin/sh
# Run from the worktree through /tmp/umvl-cpu/run. No installation needed.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
record=dev/research/evaluations/2026-11-08-gb2-cert-data
for name in modes budget; do
 c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$bh" "$record/$name.cpp" -o "build/scratch/gb2-$name"
 "build/scratch/gb2-$name" > "$record/$name.txt"
done
python3 "$record/oracle.py" > "$record/oracle.json"
python3 "$record/high_precision.py" > "$record/high-precision.json"
