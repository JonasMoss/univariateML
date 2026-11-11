#!/bin/bash
set -eu
h=dev/research/evaluations/2026-11-11-dpqr-extreme-data
python3 "$h/finalize-metadata.py"
python3 "$h/logit-smoke.py"
python3 "$h/binomial-smoke.py"
python3 "$h/skew-moments.py"
python3 "$h/skew-smoke.py"
python3 "$h/supplement-baseline.py"
python3 "$h/seam-row-replay.py"
PYTHONPATH=dev/validation python3 -m unittest quality.test_truth_extreme
python3 "$h/integrity.py"
python3 "$h/coverage.py"
python3 "$h/external-families.py"
python3 "$h/skew-overflow.py"
python3 "$h/subnormal-forward.py"
python3 "$h/summarize.py"
