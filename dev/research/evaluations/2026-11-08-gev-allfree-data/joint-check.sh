#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/scratch "$TMPDIR"
python3 - <<'PY'
import json,pathlib
s=[]
for family in ['gev','frechet']:
 g=json.loads(pathlib.Path(f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
 s.append('const std::vector<double> '+family+'_golden{'+','.join(map(repr,g['data']))+'};')
pathlib.Path('build/scratch/evt-joint-golden.hpp').write_text('\n'.join(s))
PY
c++ -std=c++17 -O2 -IunivariateML-cpp/include -Ibuild/scratch dev/research/evaluations/2026-11-08-gev-allfree-data/joint-check.cpp -o build/scratch/joint-check
mkdir -p build/scratch/g1-joint-check
build/scratch/joint-check > build/scratch/g1-joint-check/joint-bounds.json
UMVL_JOINT_OUTPUT=build/scratch/g1-joint-check python3 dev/research/evaluations/2026-11-08-gev-allfree-data/joint-check.py > build/scratch/g1-joint-check/joint-independent.json
cp build/scratch/g1-joint-check/joint-*.json dev/research/evaluations/2026-11-08-gev-allfree-data/
