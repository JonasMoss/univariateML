#!/bin/sh
# Reserve all workstation slots through run4 before invoking.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/bnbinom-alpha
for suite in cpp-no-long-double js-build js-test site-check; do
  echo "START workstation-$suite $(date -u +%FT%TZ)"
  case "$suite" in
    cpp-no-long-double) just cpp-test-no-long-double;;
    *) just "$suite";;
  esac
  echo "GREEN workstation-$suite $(date -u +%FT%TZ)"
done
