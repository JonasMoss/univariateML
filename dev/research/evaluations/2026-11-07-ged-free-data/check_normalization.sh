#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_dir" dev/research/evaluations/2026-11-07-ged-free-data/check_normalization.cpp -o build/scratch/check_normalization
python3 - <<'PY'
import json
from pathlib import Path
x=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/ged/golden.json').read_text())['data']
Path('build/scratch/ged-data.txt').write_text('\n'.join(map(str,x)))
PY
build/scratch/check_normalization < build/scratch/ged-data.txt
