#!/usr/bin/env bash
# Recreate the native before measurements from the task's frozen base.
# Invoke through /tmp/umvl-cpu/run from the repository root.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
h=dev/research/evaluations/2026-11-10-acc-newton-data
base=build/scratch/acc-before-source
mkdir -p "$base" "$TMPDIR"
git archive c60d92ea univariateML-cpp/include bindings/common | tar -x -C "$base"
python3 "$h/capture.py" > build/scratch/acc-before.cpp
python3 "$h/replay.py" > build/scratch/acc-before-replay.cpp
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
for name in capture replay; do
 source=build/scratch/acc-before.cpp
 dest="$h/before-native.jsonl"
 if [ "$name" = replay ]; then source=build/scratch/acc-before-replay.cpp;dest="$h/replay-before.jsonl";fi
 c++ -O1 -std=c++17 -I"$base" -I"$base/univariateML-cpp/include" -I"$boost" "$source" -o "build/scratch/acc-before-$name"
 "build/scratch/acc-before-$name" > "$dest"
done
