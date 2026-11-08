#!/usr/bin/env bash
set -euo pipefail
export CMAKE_BUILD_PARALLEL_LEVEL=${BHATT_TEST_CPUS:-4} CTEST_PARALLEL_LEVEL=${BHATT_TEST_CPUS:-4}
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch
if [[ ${BHATT_TEST_CPUS:-4} == 1 ]]; then
  cpus=$(python3 -c 'import os; print(min(os.sched_getaffinity(0)))')
  taskset -pc "$cpus" $$
fi
printf "Machine: "
hostname
g++ --version | head -1
Rscript -e 'cat(R.version.string,"\n")'
for tier in dev full; do
  UMVL_TEST_TIER="$tier" just cpp-test > "build/scratch/final-cpp-$tier.log" 2>&1
  tail -12 "build/scratch/final-cpp-$tier.log"
done
just cpp-test-no-long-double > build/scratch/final-no-long-double.log 2>&1
tail -12 build/scratch/final-no-long-double.log
if [[ ${BHATT_CPP_ONLY:-0} == 1 ]]; then exit 0; fi
just test > build/scratch/final-r-dev.log 2>&1
tail -12 build/scratch/final-r-dev.log
just test-full > build/scratch/final-r-full.log 2>&1
tail -12 build/scratch/final-r-full.log
