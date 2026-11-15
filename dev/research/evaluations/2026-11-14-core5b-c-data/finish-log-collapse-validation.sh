#!/bin/bash
set -eu
D=dev/research/evaluations/2026-11-14-core5b-c-data
/tmp/umvl-cpu/run bash "$D/final-snapshot-checks.sh" > "$D/final-snapshot-checks.log" 2>&1
bash "$D/log-collapse-judges.sh"
