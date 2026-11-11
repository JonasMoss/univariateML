#!/usr/bin/env bash
# Invoke through /tmp/umvl-cpu/run; no nested reservations.
set -euo pipefail
P=dev/research/evaluations/2026-11-11-acc-fb-count-data
LOG=build/scratch/acc-fb-count-logs
python3 -B "$P/check-spots.py" > "$LOG/binding-references.log" 2>&1
python3 -B "$P/assess.py" > "$LOG/assessment.log" 2>&1
python3 -B "$P/make-report.py"
python3 -B "$P/check-report-links.py" > "$LOG/markdown-links.log" 2>&1
python3 -B "$P/audit-final.py" > "$LOG/final-audit.log" 2>&1
