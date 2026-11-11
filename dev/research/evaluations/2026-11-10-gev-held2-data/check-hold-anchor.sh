#!/usr/bin/env bash
# One reserved slot, sequential computation; official sources are untouched.
set -euo pipefail
export TMPDIR=$PWD/build/tmp
b=$(Rscript -e 'cat(system.file("include",package="BH"))')
d=dev/research/evaluations/2026-11-10-gev-held2-data
python3 - <<'PY'
import shutil
from pathlib import Path
p=Path('build/scratch/hold-anchor-include')
shutil.copytree('univariateML-cpp/include',p,dirs_exist_ok=True)
f=p/'cppunivariate/distributions/gev/mle.hpp'
s=f.read_text()
a='''  const double origin=y[0];
  const double scale=hs ? sigma : std::max(y.back()-origin,std::abs(origin-mu));'''
b='''  const double origin=hm ? mu : y[0];
  const double scale=hs ? sigma : std::max({y.back()-y[0],std::abs(y[0]-origin),std::abs(y.back()-origin)});'''
assert s.count(a)==1
f.write_text(s.replace(a,b))
PY
g++ -std=c++17 -O2 -Ibuild/scratch/hold-anchor-include -I"$b" "$d/open-cases.cpp" -o build/scratch/open-anchor
build/scratch/open-anchor > "$d/open-anchor.csv"
