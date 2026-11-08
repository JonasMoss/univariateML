#!/bin/bash
# Workstation: reserve only the computing commands, never nest locks.
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
D=dev/research/evaluations/2026-11-08-sstd-free-data
/tmp/umvl-cpu/run bash "$D/build-baseline.sh"
/tmp/umvl-cpu/run bash "$D/targeted.sh"
/tmp/umvl-cpu/run bash "$D/benchmark.sh"
/tmp/umvl-cpu/run python3 "$D/check.py"
# Long validation is separately invoked via validate-simbox.sh / validate-local.sh.
/tmp/umvl-cpu/run python3 "$D/check-side-scales.py"
/tmp/umvl-cpu/run bash "$D/moment.sh"
