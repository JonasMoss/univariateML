#!/bin/sh
# Invoke through run and taskset to one allowed CPU; browser threads inherit it.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
echo "START workstation-site-check $(date -u +%FT%TZ)"
just site-check
echo "GREEN workstation-site-check $(date -u +%FT%TZ)"
