#!/bin/sh
# Invoke through run4; never acquire nested CPU slot locks.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch/bnbinom-allfree
for suite in cpp-test-no-long-double js-build js-test site-check; do
  echo "START workstation-$suite $(date -u +%FT%TZ)"
  just "$suite"
  echo "GREEN workstation-$suite $(date -u +%FT%TZ)"
done
