#!/bin/bash
set -eu
run=dev/research/evaluations/2026-11-07-budget-align-data/run.py
python3 "$run" cpp-full-final10 simbox env UMVL_TEST_TIER=full just cpp-test
python3 "$run" cpp-dev-final10 simbox just cpp-test
python3 "$run" cpp-no-long-double-final10 simbox just cpp-test-no-long-double
