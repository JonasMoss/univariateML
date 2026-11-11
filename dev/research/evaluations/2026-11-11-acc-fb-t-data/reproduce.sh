#!/usr/bin/env bash
# Frozen baseline bc8ff7be; no git state changes, installs or source archives kept.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/baseline build/scratch
baseline=bc8ff7be2e3bc4b9186a56fc4f2e8ee45c163714
git archive "$baseline" univariateML-cpp/include bindings/common | tar -x -C build/baseline
D=dev/research/evaluations/2026-11-11-acc-fb-t-data
for phase in before after; do
 for mode in native double; do
  /tmp/umvl-cpu/run bash "$D/build-capture.sh" "$mode" "$phase"
 done
done
/tmp/umvl-cpu/run python3 "$D/oracle.py"
/tmp/umvl-cpu/run python3 "$D/compare.py"

/tmp/umvl-cpu/run python3 "$D/verify.py"
