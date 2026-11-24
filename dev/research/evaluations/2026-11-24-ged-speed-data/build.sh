#!/bin/sh
# From the repository root, run through /tmp/umvl-cpu/run.
set -eu
previous=dev/research/evaluations/2026-11-22-pged-tail-data
data=dev/research/evaluations/2026-11-24-ged-speed-data
work=build/scratch/ged-speed
include=univariateML-cpp/include
boost=${BOOST_INCLUDE:-$(Rscript -e 'cat(system.file("include", package="BH"))')}
if [ "${1:-}" != symmetric ]; then
    for spec in baseline:b6a00871 master:0480604f; do
        variant=${spec%:*}
        mkdir -p "$work/$variant"
        git archive "${spec#*:}" "$include" | tar -x -C "$work/$variant"
    done
fi
mkdir -p "$work/bin" "$work/symmetric"
for mode in native double; do
    flag=
    if [ "$mode" = double ]; then flag=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS; fi
    if [ "${1:-}" != symmetric ]; then
        for variant in baseline master gated; do
            headers=$include
            if [ "$variant" != gated ]; then headers="$work/$variant/$include"; fi
            "${CXX:-g++}" -std=c++17 -O2 $flag -DALTERNATIVE_RADIUS=0 -I "$boost" -I "$headers" \
                "$previous/probe.cpp" -o "$work/bin/$variant-$mode"
        done
    fi
    "${CXX:-g++}" -std=c++17 -O2 $flag -I "$boost" -I "$include" \
        "$data/symmetric.cpp" -o "$work/symmetric/gated-$mode"
done
