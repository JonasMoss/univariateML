#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
mkdir -p build/tmp build/scratch
# Default to three CPUs when overlapping the one-CPU simbox batch.
# BHATT_LOCAL_CPUS=4 uses the complete reservation for a sequential run.
export BHATT_LOCAL_CPUS=${BHATT_LOCAL_CPUS:-3}
bhatt_cpus=$(python3 -c 'import os; print(",".join(map(str,sorted(os.sched_getaffinity(0))[:int(os.environ["BHATT_LOCAL_CPUS"])])))')
taskset -pc "$bhatt_cpus" $$
export CMAKE_BUILD_PARALLEL_LEVEL=$BHATT_LOCAL_CPUS CTEST_PARALLEL_LEVEL=$BHATT_LOCAL_CPUS EMCC_CORES=$BHATT_LOCAL_CPUS
printf 'Machine: '
hostname
g++ --version | head -1
just cpp-test-no-long-double > build/scratch/final-local-no-long-double.log 2>&1
tail -12 build/scratch/final-local-no-long-double.log
just js-build > build/scratch/final-js-build.log 2>&1
tail -8 build/scratch/final-js-build.log
just js-test > build/scratch/final-js-test.log 2>&1
tail -12 build/scratch/final-js-test.log
just site-check > build/scratch/final-site-check.log 2>&1
tail -15 build/scratch/final-site-check.log
