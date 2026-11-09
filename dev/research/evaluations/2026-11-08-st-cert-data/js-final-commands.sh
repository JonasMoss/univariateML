#!/usr/bin/env bash
# Caller reserves one CPU and pins this process and all children to that CPU.
set -euo pipefail
export EMCC_CORES=1 BINARYEN_CORES=1
out=dev/research/evaluations/2026-11-08-st-cert-data
bash "$out/postfix-evidence.sh"
printf 'Workstation JS build: one pinned CPU, EMCC_CORES=1, BINARYEN_CORES=1\n' > "$out/js-build.txt"
just js-build >> "$out/js-build.txt" 2>&1
just js-test > "$out/js-test.txt" 2>&1
just site-check > "$out/site-check.txt" 2>&1
