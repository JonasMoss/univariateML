#!/usr/bin/env bash
set -u
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export MAKEFLAGS=-j4
mkdir -p "$TMPDIR" build/scratch/oracle-final
logdir="$PWD/build/scratch/oracle-final"
: > "$logdir/receipts.txt"
failed=0
fallback=()
names=(cpp-dev r-dev r-full cpp-double)
recipes=(cpp-test test test-full cpp-test-no-long-double)
for i in 0 1 2 3; do
  name="${names[$i]}"
  /tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 just "${recipes[$i]}" >"$logdir/$name-simbox.log" 2>&1
  rc=$?
  echo "$name simbox $rc" | tee -a "$logdir/receipts.txt"
  if [ "$rc" = 75 ]; then
    if [ "$name" != cpp-double ]; then fallback+=("$name"); fi
  elif [ "$rc" != 0 ]; then
    failed=1
  fi
done
# All waiting for simbox is over before reserving workstation slots. These
# are parallel builds/suites, run sequentially in one foreground reservation.
fallback+=(cpp-double site)
/tmp/umvl-cpu/run4 bash dev/research/evaluations/2026-11-11-oracle-gaps-data/local_suites.sh "${fallback[@]}"
rc=$?
if [ "$rc" != 0 ]; then failed=1; fi
exit "$failed"
