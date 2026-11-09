#!/usr/bin/env bash
set -euo pipefail
exec /tmp/umvl-cpu/run bash -c '
 set -euo pipefail
 cpu=$(python3 -c "import os; print(min(os.sched_getaffinity(0)))")
 printf "Workstation JS build: one CPU, EMCC_CORES=1, BINARYEN_CORES=1\n"
 exec taskset -c "$cpu" env EMCC_CORES=1 BINARYEN_CORES=1 just js-build
'
