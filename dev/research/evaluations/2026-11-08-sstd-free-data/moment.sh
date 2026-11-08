#!/bin/bash
set -euo pipefail
D=dev/research/evaluations/2026-11-08-sstd-free-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -I"$boost" -IunivariateML-cpp/include "$D/moment-probe.cpp" -o build/scratch/moment-probe
build/scratch/moment-probe > "$D/moment-probe.txt"
python3 "$D/check-moment.py" > "$D/moment-checks.txt"
