#!/usr/bin/env bash
set -euo pipefail
out=dev/research/evaluations/2026-11-08-st-cert-data
just cpp-test > "$out/cpp-dev.txt.gz" 2>&1
UMVL_TEST_TIER=full just cpp-test > "$out/cpp-full.txt.gz" 2>&1
just cpp-test-no-long-double > "$out/cpp-no-long-double-simbox.txt.gz" 2>&1
just test > "$out/r-dev.txt.gz" 2>&1
just test-full > "$out/r-full.txt.gz" 2>&1
