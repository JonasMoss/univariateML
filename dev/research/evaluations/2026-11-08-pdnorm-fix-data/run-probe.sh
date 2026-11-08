#!/usr/bin/env bash
# From the repository root: /tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-08-pdnorm-fix-data/run-probe.sh
set -euo pipefail
cd "$(dirname "$0")/../../../.."
mkdir -p build/scratch
probe_data=dev/research/evaluations/2026-11-08-pdnorm-fix-data
python3 "$probe_data/check.py" > "$probe_data/reference.json"
probe_boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
flags=(-std=c++17 -O2 -IunivariateML-cpp/include -I"$probe_boost")
g++ --version | head -1
g++ "${flags[@]}" "$probe_data/probe.cpp" -o build/scratch/pdnorm-probe
echo 'Native long-double backend:'
build/scratch/pdnorm-probe
g++ "${flags[@]}" -DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS "$probe_data/probe.cpp" -o build/scratch/pdnorm-probe-double
echo 'No-long-double backend:'
build/scratch/pdnorm-probe-double
