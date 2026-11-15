#!/bin/sh
# Build the sweep evaluator against the baseline headers (master 2c62400a)
# and against the working tree. Run from the repository root:
#   sh dev/research/evaluations/2026-11-14-sged-tail-data/build.sh [baseline|current]
set -e
data=dev/research/evaluations/2026-11-14-sged-tail-data
boost=${BOOST_INCLUDE:-$HOME/R/x86_64-pc-linux-gnu-library/4.6/BH/include}
mkdir -p build/sged-tail
case "${1:-current}" in
  baseline)
    rm -rf build/sged-tail/baseline && mkdir -p build/sged-tail/baseline
    git archive 2c62400a univariateML-cpp/include | tar -x -C build/sged-tail/baseline
    include=build/sged-tail/baseline/univariateML-cpp/include ;;
  current) include=univariateML-cpp/include; flags=-DSGED_TAIL_CURRENT ;;
esac
g++ -std=c++17 -O2 $flags -I "$include" -I "$boost" "$data/evaluate.cpp" -o "build/sged-tail/eval-${1:-current}"
