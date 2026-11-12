#!/usr/bin/env bash
# Both single-threaded consumers share one foreground run reservation.
set -eu
prefix="$1"
shift
python3 dev/research/evaluations/2026-11-11-oracle-gaps-data/audit.py check "$prefix-native" "$@" >"build/scratch/oracle-consumers/$prefix-native.log" 2>&1
python3 dev/research/evaluations/2026-11-11-oracle-gaps-data/audit.py check "$prefix-double" "$@" >"build/scratch/oracle-consumers/$prefix-double.log" 2>&1
