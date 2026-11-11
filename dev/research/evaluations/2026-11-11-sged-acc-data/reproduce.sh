#!/bin/bash
set -euo pipefail
# Run from repository root. Dependencies are the existing compiler, BH, mpmath.
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
D=dev/research/evaluations/2026-11-11-sged-acc-data
/tmp/umvl-cpu/run python3 "$D/prepare.py"
for phase in before after;do
 for mode in native double;do
  /tmp/umvl-cpu/run bash "$D/replay.sh" "$mode" "$phase"
  /tmp/umvl-cpu/run python3 "$D/check.py" "$phase" "$mode"
 done
done
/tmp/umvl-cpu/run python3 "$D/certified.py"
/tmp/umvl-cpu/run python3 "$D/comparison.py"
/tmp/umvl-cpu/run python3 "$D/summary.py"
/tmp/umvl-cpu/run python3 "$D/verify.py"
/tmp/umvl-cpu/run python3 "$D/subunit-reference.py"
/tmp/umvl-cpu/run python3 "$D/cusp-reference.py"
/tmp/umvl-cpu/run python3 "$D/loose-reference.py"
/tmp/umvl-cpu/run bash "$D/snorm-stops.sh"
