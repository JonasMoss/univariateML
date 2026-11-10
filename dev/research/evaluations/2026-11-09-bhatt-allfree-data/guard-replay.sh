#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
g++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-09-bhatt-allfree-data/guard-replay.cpp -o build/scratch/bhatt-guard-replay
build/scratch/bhatt-guard-replay
