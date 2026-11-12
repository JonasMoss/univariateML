#!/usr/bin/env bash
set -eu
python3 dev/research/evaluations/2026-11-11-oracle-gaps-data/smoke.py >build/scratch/oracle-consumers/fixed-smoke.log 2>&1
bash dev/research/evaluations/2026-11-11-oracle-gaps-data/run_consumers.sh "${1:-measured}" --active-only
