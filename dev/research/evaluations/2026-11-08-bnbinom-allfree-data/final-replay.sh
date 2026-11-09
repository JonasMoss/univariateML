#!/bin/sh
# Current-source replay under one CPU reservation; no nested slot wrappers.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
evidence=dev/research/evaluations/2026-11-08-bnbinom-allfree-data
sh "$evidence/benchmark.sh"
sh "$evidence/final-evidence.sh"
Rscript "$evidence/r-targeted.R"
