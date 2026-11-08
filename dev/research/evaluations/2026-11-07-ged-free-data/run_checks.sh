#!/usr/bin/env bash
set -euo pipefail
# Run from the repository root, inside run/run4 or simbox-run.
files=dev/research/evaluations/2026-11-07-ged-free-data
mkdir -p build/scratch
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ --version | head -1
sha256sum univariateML-cpp/include/cppunivariate/distributions/ged/{mle.hpp,free_shape_bounds.hpp,test.cpp,README.md} bindings/common/distributions/ged/fit.hpp bindings/r/distributions/ged/test.R bindings/js/test.mjs
python3 - <<'PY'
import json
from pathlib import Path
x=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/ged/golden.json').read_text())['data']
Path('build/scratch/ged-data.txt').write_text('\n'.join(map(str,x)))
PY
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_dir" "$files/benchmark-baseline.cpp" -o build/scratch/ged-baseline
printf 'BASELINE\n'
build/scratch/ged-baseline < build/scratch/ged-data.txt
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_dir" "$files/benchmark.cpp" -o build/scratch/ged-current
printf 'CURRENT\n'
build/scratch/ged-current < build/scratch/ged-data.txt
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_dir" "$files/check_bounds.cpp" -o build/scratch/check_bounds
python3 "$files/check_bounds.py" build/scratch/check_bounds
