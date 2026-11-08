#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch
files=dev/research/evaluations/2026-11-07-ged-free-data
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
python3 - <<'PY'
import json
from pathlib import Path
x=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/ged/golden.json').read_text())['data']
Path('build/scratch/ged-data.txt').write_text('\n'.join(map(str,x)))
PY
c++ --version | head -1
c++ -std=c++17 -O2 -DGED_O1_BASELINE -IunivariateML-cpp/include -I"$boost_dir" "$files/o1-timings.cpp" -o build/scratch/o1-time-before
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_dir" "$files/o1-timings.cpp" -o build/scratch/o1-time-after
printf 'BEFORE\n'
build/scratch/o1-time-before < build/scratch/ged-data.txt
printf 'AFTER\n'
build/scratch/o1-time-after < build/scratch/ged-data.txt
