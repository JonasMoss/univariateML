#!/bin/sh
# Run from the worktree root through run (one slot).
set -eu
export TMPDIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/bnbinom-alpha/base/cppunivariate/distributions/bnbinom
base="$PWD/build/scratch/bnbinom-alpha/base/cppunivariate"
git show d34a44cd:univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp > "$base/distributions/bnbinom/mle.hpp"
for d in core special; do ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/$d" "$base/$d"; done
for d in nbinom pois; do ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/distributions/$d" "$base/distributions/$d"; done
ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/distributions/bnbinom/dpqr.hpp" "$base/distributions/bnbinom/dpqr.hpp"
python3 - <<'PY'
import json
from pathlib import Path
x=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json').read_text())['data']
Path('build/scratch/bnbinom-alpha/counts.txt').write_text('\n'.join(map(str,x)))
PY
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
evidence=dev/research/evaluations/2026-11-07-bnbinom-alpha-data
c++ -O2 -std=c++17 -Ibuild/scratch/bnbinom-alpha/base -IunivariateML-cpp/include -I"$bh" "$evidence/benchmark.cpp" -o build/scratch/bnbinom-alpha/before
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$bh" "$evidence/benchmark.cpp" -o build/scratch/bnbinom-alpha/after
for pair in 1 2 3; do
  build/scratch/bnbinom-alpha/before build/scratch/bnbinom-alpha/counts.txt > "$evidence/before-$pair.csv"
  build/scratch/bnbinom-alpha/after build/scratch/bnbinom-alpha/counts.txt > "$evidence/after-$pair.csv"
done
python3 "$evidence/summarize.py"
