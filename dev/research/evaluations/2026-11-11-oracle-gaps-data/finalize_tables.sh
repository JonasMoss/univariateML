#!/usr/bin/env bash
set -eu
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
# Single-threaded mathematical and metadata work; no nested reservations.
/tmp/umvl-cpu/run bash -c '
set -eu
p=dev/research/evaluations/2026-11-11-oracle-gaps-data
python3 "$p/reprobe_archived.py" >build/scratch/oracle-final/open-probes.log 2>&1
python3 "$p/classify_rows.py" measured-closure
python3 "$p/preserve.py"
python3 "$p/refresh_receipts.py"
python3 "$p/diagnose.py"
python3 "$p/summarize.py" >build/scratch/oracle-final/summary.log
python3 "$p/python_gate.py" >build/scratch/oracle-final/python-gate.log 2>&1
'
