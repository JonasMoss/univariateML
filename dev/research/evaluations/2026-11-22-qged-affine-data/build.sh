#!/bin/sh
set -eu
work=build/scratch/qged
data=dev/research/evaluations/2026-11-22-qged-affine-data
boost=${BOOST_INCLUDE:-$(Rscript -e 'cat(system.file("include", package="BH"))')}
mkdir -p "$work/baseline"
git archive 7a57d011 univariateML-cpp/include | tar -x -C "$work/baseline"
for variant in "$@"; do
    for mode in native double; do
        flag=
        if [ "$mode" = double ]; then flag=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS; fi
        include=univariateML-cpp/include
        source="$data/probe.cpp"
        if [ "$variant" = baseline ]; then
            include="$work/baseline/$include"
            source=dev/research/evaluations/2026-11-22-pged-tail-data/probe.cpp
        fi
        "${CXX:-g++}" -std=c++17 -O2 $flag -I "$boost" -I "$include" \
            "$source" -o "$work/$variant-$mode"
    done
done
