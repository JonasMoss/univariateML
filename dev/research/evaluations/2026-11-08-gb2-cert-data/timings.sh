#!/bin/sh
# Reserve one slot with /tmp/umvl-cpu/run before invoking from repository root.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
record=dev/research/evaluations/2026-11-08-gb2-cert-data
baseline=build/scratch/gb2-before
mkdir -p "$baseline"
# Read-only git command: reconstruct the untouched base headers in scratch.
git archive f204b94e univariateML-cpp/include | tar -x -C "$baseline"
GB2_INCLUDE="$baseline/univariateML-cpp/include" sh "$record/check.sh" > "build/scratch/gb2-before-$$.txt"
mv "build/scratch/gb2-before-$$.txt" "$record/before.txt"
GB2_INCLUDE=univariateML-cpp/include sh "$record/check.sh" > "build/scratch/gb2-after-$$.txt"
mv "build/scratch/gb2-after-$$.txt" "$record/after.txt"
