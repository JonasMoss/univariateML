#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_dir" dev/research/evaluations/2026-11-07-ged-free-data/o1-probe.cpp -o build/scratch/o1-probe
python3 - <<'PY'
import json
from pathlib import Path
x=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/ged/golden.json').read_text())['data']
Path('build/scratch/ged-data.txt').write_text('\n'.join(map(str,x)))
PY
build/scratch/o1-probe < build/scratch/ged-data.txt
