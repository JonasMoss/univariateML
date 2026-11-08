#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1 MAKEFLAGS=-j4
mkdir -p build/tmp build/scratch
printf 'Machine: '
hostname
Rscript -e 'cat(R.version.string,"\n")'
just test > build/scratch/final-r-dev.log 2>&1
tail -12 build/scratch/final-r-dev.log
just test-full > build/scratch/final-r-full.log 2>&1
tail -12 build/scratch/final-r-full.log
