#!/bin/bash
set -eu
run=dev/research/evaluations/2026-11-07-budget-align-data/run.py
python3 "$run" r-dev-final10 simbox just test
python3 "$run" r-full-final10 simbox just test-full
