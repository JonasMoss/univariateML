#!/bin/bash
set -eu
run=dev/research/evaluations/2026-11-07-budget-align-data/run.py
python3 "$run" js-build-final10 workstation env EMCC_CORES=1 just js-build
python3 "$run" js-test-final10 workstation just js-test
python3 "$run" site-check-final10 workstation just site-check
