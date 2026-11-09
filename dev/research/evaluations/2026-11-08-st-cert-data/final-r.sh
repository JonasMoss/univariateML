#!/usr/bin/env bash
set -euo pipefail
out=dev/research/evaluations/2026-11-08-st-cert-data
just test > "$out/r-dev.txt.gz" 2>&1
just test-full > "$out/r-full.txt.gz" 2>&1
