#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export GIT_OPTIONAL_LOCKS=0 PYTHONDONTWRITEBYTECODE=1
/tmp/umvl-cpu/run4 bash build/scratch/measure-strict.sh cold
/tmp/umvl-cpu/run4 bash build/scratch/measure-strict.sh warm
/tmp/umvl-cpu/run bash build/scratch/measure-strict.sh warm-serial
/tmp/umvl-cpu/run python3 build/scratch/touch-snapshots.py
/tmp/umvl-cpu/run4 bash build/scratch/measure-strict.sh touched
/tmp/umvl-cpu/run python3 build/scratch/compare-strict.py \
  build/scratch dev/research/evaluations/2026-11-21-strict-par-data
