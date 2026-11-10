#!/usr/bin/env bash
# Run from the repository root. Requires the existing workstation toolchain.
# Each compute command reserves slots; no reservation is nested.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
accuracy_data=dev/research/evaluations/2026-11-10-acc-profile-data
accuracy_boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
python3 "$accuracy_data/capture.py" > build/scratch/acc-profile-capture.cpp
python3 "$accuracy_data/replay.py" > build/scratch/acc-profile-replay.cpp
for accuracy_mode in native double; do
 accuracy_flags=();if [[ $accuracy_mode == double ]];then accuracy_flags=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS);fi
 /tmp/umvl-cpu/run c++ -std=c++17 -O2 "${accuracy_flags[@]}" -I. -IunivariateML-cpp/include -I"$accuracy_boost" build/scratch/acc-profile-capture.cpp -o "build/scratch/acc-profile-$accuracy_mode"
 /tmp/umvl-cpu/run "build/scratch/acc-profile-$accuracy_mode" > "$accuracy_data/after-$accuracy_mode.jsonl"
 /tmp/umvl-cpu/run c++ -std=c++17 -O2 "${accuracy_flags[@]}" -IunivariateML-cpp/include -I"$accuracy_boost" "$accuracy_data/boundary-rounding.cpp" -o "build/scratch/acc-profile-boundary-$accuracy_mode"
 /tmp/umvl-cpu/run "build/scratch/acc-profile-boundary-$accuracy_mode" > "$accuracy_data/boundary-$accuracy_mode.jsonl"
 /tmp/umvl-cpu/run c++ -std=c++17 -O2 "${accuracy_flags[@]}" -IunivariateML-cpp/include -I"$accuracy_boost" "$accuracy_data/replication.cpp" -o "build/scratch/acc-profile-replication-$accuracy_mode"
 /tmp/umvl-cpu/run "build/scratch/acc-profile-replication-$accuracy_mode" > "$accuracy_data/replication-$accuracy_mode.json"
done
/tmp/umvl-cpu/run c++ -std=c++17 -O2 -I. -IunivariateML-cpp/include -I"$accuracy_boost" build/scratch/acc-profile-replay.cpp -o build/scratch/acc-profile-replay
/tmp/umvl-cpu/run build/scratch/acc-profile-replay > "$accuracy_data/replay-after.jsonl"
/tmp/umvl-cpu/run4 just regen
/tmp/umvl-cpu/run env MAKEFLAGS=-j1 Rscript "$accuracy_data/capture.R" > "$accuracy_data/after-r.jsonl"
/tmp/umvl-cpu/run env MAKEFLAGS=-j1 Rscript "$accuracy_data/replication.R" > "$accuracy_data/replication-r.json"
/tmp/umvl-cpu/run4 just js-build
/tmp/umvl-cpu/run node --v8-pool-size=1 "$accuracy_data/capture.mjs" > "$accuracy_data/after-wasm.jsonl"
/tmp/umvl-cpu/run node --v8-pool-size=1 "$accuracy_data/replication.mjs" > "$accuracy_data/replication-wasm.json"
/tmp/umvl-cpu/run python3 "$accuracy_data/verify.py"
/tmp/umvl-cpu/run python3 "$accuracy_data/analyze.py"
