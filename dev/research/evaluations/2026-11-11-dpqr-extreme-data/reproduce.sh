#!/bin/bash
# Run directly, from the worktree root: this script reserves slots per command.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
h=dev/research/evaluations/2026-11-11-dpqr-extreme-data
# Exit one records explicit reference gaps. Other failures abort.
rc=0
/tmp/umvl-cpu/run4 env TRUTH_EXTREME_WORKERS=4 TRUTH_EXTREME_DIGITS=160 TRUTH_EXTREME_TIMEOUT=120 python3 "$h/generate.py" || rc=$?
test "$rc" -le 1
/tmp/umvl-cpu/run python3 "$h/finalize-metadata.py"
/tmp/umvl-cpu/run python3 "$h/audit.py" build baseline
/tmp/umvl-cpu/run python3 "$h/audit.py" build current
/tmp/umvl-cpu/run python3 "$h/audit.py" check before
/tmp/umvl-cpu/run python3 "$h/audit.py" check after
/tmp/umvl-cpu/run python3 "$h/priority.py" after
/tmp/umvl-cpu/run python3 "$h/weibull-center.py" after
/tmp/umvl-cpu/run python3 "$h/smoke.py"
/tmp/umvl-cpu/run python3 "$h/density-smoke.py"
/tmp/umvl-cpu/run python3 "$h/beta-smoke.py"
/tmp/umvl-cpu/run python3 "$h/gamma-probability.py"
/tmp/umvl-cpu/run python3 "$h/beta-two.py"
/tmp/umvl-cpu/run python3 "$h/fatigue-center.py" after
/tmp/umvl-cpu/run python3 "$h/fatigue-underflow.py"
/tmp/umvl-cpu/run python3 "$h/power-smoke.py"
/tmp/umvl-cpu/run python3 "$h/skew-compensation.py"
/tmp/umvl-cpu/run python3 "$h/skew-gate.py"
/tmp/umvl-cpu/run python3 "$h/samplers.py"
/tmp/umvl-cpu/run python3 "$h/stats.py"
/tmp/umvl-cpu/run python3 "$h/seam.py" final
/tmp/umvl-cpu/run python3 "$h/integrity.py"
/tmp/umvl-cpu/run python3 "$h/coverage.py"
/tmp/umvl-cpu/run bash "$h/final-supplements.sh"
bash "$h/final-runs.sh"
