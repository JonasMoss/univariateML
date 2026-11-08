#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
just cpp-test-no-long-double > build/scratch/accept-local-no-long-double.log 2>&1
just js-build > build/scratch/accept-js-build.log 2>&1
just js-test > build/scratch/accept-js-test.log 2>&1
just site-check > build/scratch/accept-site.log 2>&1
for path in build/scratch/accept-{local-no-long-double,js-build,js-test,site}.log; do
  echo "$path"
  tail -n 12 "$path"
done
