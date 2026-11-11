#!/usr/bin/env bash
# Run from the repository root without an outer CPU wrapper: each computing
# command below reserves its own slot. No package installation or Git writes.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
here=dev/research/evaluations/2026-11-10-acc-newton-data
/tmp/umvl-cpu/run python3 "$here/capture.py" > build/scratch/acc-capture.cpp
/tmp/umvl-cpu/run python3 "$here/replay.py" > build/scratch/acc-replay.cpp
boost=$(/tmp/umvl-cpu/run Rscript -e 'cat(system.file("include",package="BH"))')
for mode in native double; do
 flags=();[ "$mode" != double ] || flags=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS)
 /tmp/umvl-cpu/run c++ -O2 -std=c++17 "${flags[@]}" -I. -IunivariateML-cpp/include -I"$boost" build/scratch/acc-capture.cpp -o "build/scratch/acc-$mode"
 /tmp/umvl-cpu/run "build/scratch/acc-$mode" > "$here/after-$mode.jsonl"
 /tmp/umvl-cpu/run c++ -O1 -std=c++17 "${flags[@]}" -I. -IunivariateML-cpp/include -I"$boost" build/scratch/acc-replay.cpp -o "build/scratch/acc-replay-$mode"
 suffix=;[ "$mode" != double ] || suffix=-double
 /tmp/umvl-cpu/run "build/scratch/acc-replay-$mode" > "$here/replay-after$suffix.jsonl"
 /tmp/umvl-cpu/run c++ -O1 -std=c++17 "${flags[@]}" -I. -IunivariateML-cpp/include -I"$boost" "$here/held-probes.cpp" -o "build/scratch/acc-held-probes-$mode"
 /tmp/umvl-cpu/run "build/scratch/acc-held-probes-$mode" > "$here/held-probes-$mode.jsonl"
done
/tmp/umvl-cpu/run python3 "$here/measure.py"
