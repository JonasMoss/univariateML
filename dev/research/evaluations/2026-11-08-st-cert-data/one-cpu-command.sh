#!/usr/bin/env bash
# Caller reserves one CPU through /tmp/umvl-cpu/run. Pin every child too.
set -euo pipefail
cpu=$(python3 -c 'import os; print(min(os.sched_getaffinity(0)))')
exec taskset -c "$cpu" "$@"
