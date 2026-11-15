#!/usr/bin/env bash
# Invoke through /tmp/umvl-cpu/run; all children run sequentially on one slot.
set -euo pipefail
D=dev/research/evaluations/2026-11-14-core5b-b-data
bash "$D/run-preflight.sh"
bash "$D/run-snapshots.sh"
bash "$D/run-judges.sh"
