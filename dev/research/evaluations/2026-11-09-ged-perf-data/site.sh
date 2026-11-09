#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export EMCC_CORES=1
mkdir -p build/tmp build/scratch
site_cpu=$(python3 -c 'import os; print(min(os.sched_getaffinity(0)))')
printf 'BEGIN just site-check\n'
/usr/bin/time -p taskset -c "$site_cpu" just site-check
printf 'GREEN just site-check\n'
