#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR=${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML} CCACHE_BASEDIR=$PWD CCACHE_NOHASHDIR=1
mode=$1; phase=$2
flags=();if [ "$mode" = double ];then flags+=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS);fi
inc=.
if [ "$phase" = before ];then
 inc=build/baseline
 if [ ! -f "$inc/univariateML-cpp/include/cppunivariate/distributions/sged/mle.hpp" ];then
  mkdir -p "$inc"
  git archive 3b2a16fed4787c71b96bf23b1cb86569973b0a9c univariateML-cpp/include bindings/common | tar -x -C "$inc"
 fi
fi
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
ccache c++ -O2 -std=c++17 "${flags[@]}" -I"$inc" -I"$inc/univariateML-cpp/include" -I"$boost" dev/research/evaluations/2026-11-11-sged-acc-data/replay.cpp -o build/scratch/replay-$mode
build/scratch/replay-$mode dev/research/evaluations/2026-11-11-sged-acc-data/inputs.txt > dev/research/evaluations/2026-11-11-sged-acc-data/$phase-$mode.jsonl
