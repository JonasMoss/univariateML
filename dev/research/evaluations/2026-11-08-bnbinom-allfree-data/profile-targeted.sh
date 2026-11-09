#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch/bnbinom-allfree
python3 - <<'PY'
import json
from pathlib import Path
x=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json').read_text())['data']
Path('build/scratch/bnbinom-allfree/counts.txt').write_text('\n'.join(map(str,x)))
PY
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$bh" dev/research/evaluations/2026-11-08-bnbinom-allfree-data/profile-targeted.cpp -o build/scratch/bnbinom-allfree/profile-targeted
build/scratch/bnbinom-allfree/profile-targeted
