#!/bin/sh
# Run outside any CPU lock; each foreground child obtains and releases slots.
set -eu
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
for suite in js-build js-test site-check test-full; do
    /tmp/umvl-cpu/run4 just "$suite" >"build/scratch/t-port-$suite.log" 2>&1
    tail -n 8 "build/scratch/t-port-$suite.log"
done
