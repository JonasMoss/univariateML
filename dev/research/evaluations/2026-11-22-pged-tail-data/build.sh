#!/bin/sh
# From the repository root, run through /tmp/umvl-cpu/run.
set -eu
source=dev/research/evaluations/2026-11-22-pged-tail-data/probe.cpp
work=build/scratch/pged
boost=${BOOST_INCLUDE:-$(Rscript -e 'cat(system.file("include", package="BH"))')}
mkdir -p "$work/baseline"
git archive b6a00871 univariateML-cpp/include | tar -x -C "$work/baseline"
for mode in native double; do
    flag=
    if [ "$mode" = double ]; then flag=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS; fi
    for variant in baseline gated twoword; do
        include=univariateML-cpp/include
        alt=0
        case "$variant" in
            baseline) include="$work/baseline/$include" ;;
            twoword) alt=1 ;;
        esac
        "${CXX:-g++}" -std=c++17 -O2 $flag -DALTERNATIVE_RADIUS=$alt -I "$boost" -I "$include" \
            "$source" -o "$work/$variant-$mode"
    done
done
