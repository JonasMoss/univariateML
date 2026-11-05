#!/bin/sh
# Run directly, outside any slot allocation. Each child command acquires
# its own allocation; the script never holds slots between commands.
set -eu
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export PYTHONDONTWRITEBYTECODE=1
test -x /tmp/umvl-cpu/run
test -x /tmp/umvl-cpu/run4
run_suite() {
    label=$1
    shift
    /tmp/umvl-cpu/run4 "$@" >"build/scratch/t-root-$label.log" 2>&1
    tail -n 8 "build/scratch/t-root-$label.log"
}
run_suite cpp-dev just cpp-test
run_suite cpp-no-long-double just cpp-test-no-long-double
run_suite r-dev just test
run_suite js-build just js-build
run_suite js-test just js-test
run_suite cpp-full env UMVL_TEST_TIER=full just cpp-test
run_suite r-full just test-full
