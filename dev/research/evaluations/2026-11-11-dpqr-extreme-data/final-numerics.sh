#!/bin/bash
# Run inside one reserved CPU slot; every subprocess is single-threaded.
set -eu
h=dev/research/evaluations/2026-11-11-dpqr-extreme-data
rc=0
TRUTH_EXTREME_DIGITS=160 TRUTH_EXTREME_TIMEOUT=120 python3 "$h/generate.py" --one fatigue || rc=$?
test "$rc" -le 1
python3 "$h/finalize-metadata.py"
python3 "$h/audit.py" build current
python3 "$h/audit.py" build current no-long-double
if [ "${UMVL_AUDIT_SKIP_BASELINE:-0}" != 1 ]; then python3 "$h/audit.py" check before; fi
python3 "$h/audit.py" check after
python3 "$h/audit.py" check after-double
python3 "$h/priority.py" after
python3 "$h/weibull-center.py" after
python3 "$h/smoke.py"
python3 "$h/density-smoke.py"
python3 "$h/beta-smoke.py"
python3 "$h/gamma-probability.py"
python3 "$h/beta-two.py"
python3 "$h/beta-two.py" --double
python3 "$h/skew-compensation.py"
python3 "$h/skew-gate.py"
python3 "$h/fatigue-center.py" before
python3 "$h/fatigue-center.py" after
python3 "$h/fatigue-underflow.py"
python3 "$h/power-smoke.py"
python3 "$h/asymmetric-smoke.py"
python3 "$h/seam.py" final
python3 "$h/samplers.py"
python3 "$h/stats.py"
python3 "$h/integrity.py"
python3 "$h/coverage.py"
