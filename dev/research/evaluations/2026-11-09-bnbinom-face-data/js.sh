#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
for suite in js-build js-test; do
  echo "START workstation $suite $(date -u +%FT%TZ)"
  just "$suite"
  echo "GREEN workstation $suite $(date -u +%FT%TZ)"
done
