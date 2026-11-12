#!/usr/bin/env bash
# Called only inside the one run4 reservation made by validate.sh.
set -u
logdir="$PWD/build/scratch/oracle-final"
failed=0
for name in "$@"; do
  case "$name" in
    cpp-dev) recipe=cpp-test;;
    r-dev) recipe=test;;
    r-full) recipe=test-full;;
    cpp-double) recipe=cpp-test-no-long-double;;
    site) recipe=site-check;;
    *) echo "Unknown suite: $name" >&2; exit 2;;
  esac
  just "$recipe" >"$logdir/$name-local.log" 2>&1
  rc=$?
  echo "$name workstation $rc" | tee -a "$logdir/receipts.txt"
  if [ "$rc" != 0 ]; then failed=1; fi
done
exit "$failed"
