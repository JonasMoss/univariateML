#!/usr/bin/env bash
# Pure compute commands; caller supplies the CPU reservation.
set -euo pipefail
export CCACHE_DIR=/home/jonas/.cache/ccache/univariateML CCACHE_BASEDIR=$PWD CCACHE_NOHASHDIR=1
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
out=dev/research/evaluations/2026-11-08-st-cert-data
ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$out/probe.cpp" -o build/scratch/st-probe
build/scratch/st-probe > "$out/probe.txt"
python3 "$out/check.py" > "$out/check-workstation.txt"
python3 "$out/golden-check.py" > "$out/golden-check.txt"
ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$out/timing.cpp" -o build/scratch/st-after
build/scratch/st-after > "$out/timing-after.txt"
ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$out/extra-timing.cpp" -o build/scratch/st-extra-after
build/scratch/st-extra-after > "$out/extra-timing-after.txt"
