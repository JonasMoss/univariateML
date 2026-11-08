#!/bin/bash
set -euo pipefail
D=dev/research/evaluations/2026-11-08-sstd-free-data
if [ ! -x build/scratch/timing-before ]; then bash "$D/build-baseline.sh"; fi
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -I"$boost" -IunivariateML-cpp/include -IunivariateML-cpp/tests "$D/timing.cpp" -o build/scratch/timing-after
for repeat in 1 2 3; do
 nice -n 10 build/scratch/timing-before > "$D/timing-before-$repeat.txt"
 nice -n 10 build/scratch/timing-after > "$D/timing-after-$repeat.txt"
done
cp "$D/timing-before-1.txt" "$D/timing-before.txt"
cp "$D/timing-after-1.txt" "$D/timing-after.txt"
