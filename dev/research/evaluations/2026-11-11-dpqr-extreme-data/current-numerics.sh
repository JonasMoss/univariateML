#!/bin/bash
set -eu
h=dev/research/evaluations/2026-11-11-dpqr-extreme-data
python3 "$h/finalize-metadata.py"
python3 "$h/audit.py" build current
python3 "$h/audit.py" build current no-long-double
python3 "$h/audit.py" check after
python3 "$h/audit.py" check after-double
python3 "$h/skew-moments.py"
python3 "$h/skew-smoke.py"
python3 "$h/logit-smoke.py"
python3 "$h/asymmetric-smoke.py"
python3 "$h/binomial-smoke.py"
python3 "$h/seam.py" final
