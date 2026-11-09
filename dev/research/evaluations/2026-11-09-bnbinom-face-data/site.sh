#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
cpu=$(python3 -c 'import os; print(min(os.sched_getaffinity(0)))')
taskset -c "$cpu" just site-check
