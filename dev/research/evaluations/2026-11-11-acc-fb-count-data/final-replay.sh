#!/usr/bin/env bash
set -euo pipefail
P=dev/research/evaluations/2026-11-11-acc-fb-count-data
mode=$1
python3 "$P/replay.py" "build/scratch/final-$mode" "final-count-$mode.tsv" --end 1073
python3 "$P/replay.py" "build/scratch/final-$mode" "extra-after-$mode.tsv" --start 1245
