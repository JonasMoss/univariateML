#!/bin/bash
set -eu
run=dev/research/evaluations/2026-11-07-budget-align-data/run.py
python3 "$run" cpp-dev-final11 workstation4 just cpp-test
python3 "$run" cpp-full-final11 workstation4 env UMVL_TEST_TIER=full just cpp-test
python3 "$run" cpp-no-long-double-final11 workstation4 just cpp-test-no-long-double
python3 "$run" r-dev-final11 workstation4 just test
python3 "$run" r-full-final11 workstation4 just test-full
python3 "$run" js-build-final11 workstation4 env EMCC_CORES=4 just js-build
python3 "$run" js-test-final11 workstation4 just js-test
python3 "$run" site-check-final11 workstation4 just site-check
