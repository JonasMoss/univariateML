#!/usr/bin/env bash
set -euo pipefail
# Override simbox-run's six-core defaults: this job uses at most four cores.
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
just cpp-test > build/scratch/accept-cpp-dev.log 2>&1
UMVL_TEST_TIER=full just cpp-test > build/scratch/accept-cpp-full.log 2>&1
just cpp-test-no-long-double > build/scratch/accept-no-long-double.log 2>&1
just test > build/scratch/accept-r-dev.log 2>&1
just test-full > build/scratch/accept-r-full.log 2>&1
for path in build/scratch/accept-{cpp-dev,cpp-full,no-long-double,r-dev,r-full}.log; do
  echo "$path"
  tail -n 12 "$path"
done
