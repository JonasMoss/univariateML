#!/usr/bin/env bash
# Run from the worktree root through /tmp/umvl-cpu/run (one thread).
# The R suites must have finished building univariateML-r first.
set -euo pipefail
data=dev/research/evaluations/2026-11-11-quantile-rule-data
python3 "$data/audit.py" build
python3 "$data/audit.py" build double
for mode in native double; do
  python3 "$data/audit.py" measure "$mode"
  python3 "$data/audit.py" verify "$mode"
  python3 "$data/audit.py" verify "$mode-probe"
done
node --v8-pool-size=1 "$data/audit-js.mjs"
python3 "$data/audit.py" verify js
python3 "$data/audit.py" verify js-probe
Rscript "$data/audit-r.R"
python3 "$data/audit.py" verify r
python3 "$data/audit.py" verify r-probe
python3 "$data/audit.py" provenance
