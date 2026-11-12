#!/usr/bin/env bash
set -eu
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run bash -c '
set -eu
p=dev/research/evaluations/2026-11-11-oracle-gaps-data
python3 "$p/refresh_receipts.py"
python3 "$p/diagnose.py"
python3 "$p/summarize.py" >build/scratch/oracle-final/summary.log
'
