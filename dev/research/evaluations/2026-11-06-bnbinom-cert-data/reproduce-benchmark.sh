#!/bin/sh
# From the worktree root, invoke through /tmp/umvl-cpu/run.
set -eu
export TMPDIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/bnbinom-replay/base/cppunivariate/distributions/bnbinom
replay="$PWD/build/scratch/bnbinom-replay"
base="$replay/base/cppunivariate"
git show bb734cd78b846a565d8835606328bb1285a82db6:univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp > "$base/distributions/bnbinom/mle.hpp"
for dir in core special; do ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/$dir" "$base/$dir"; done
for dir in nbinom pois; do ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/distributions/$dir" "$base/distributions/$dir"; done
ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/distributions/bnbinom/dpqr.hpp" "$base/distributions/bnbinom/dpqr.hpp"
python3 - <<'PY'
import json
from pathlib import Path
x=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json').read_text())['data']
Path('build/scratch/bnbinom-replay/counts.txt').write_text('\n'.join(map(str,x))+'\n')
PY
boost_include=$(Rscript -e 'cat(system.file("include",package="BH"))')
evidence=dev/research/evaluations/2026-11-06-bnbinom-cert-data
c++ -O2 -std=c++17 -I"$replay/base" -IunivariateML-cpp/include -I"$boost_include" "$evidence/benchmark.cpp" -o "$replay/before"
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$boost_include" "$evidence/benchmark.cpp" -o "$replay/after"
for pair in 1 2 3; do
  "$replay/before" "$replay/counts.txt" > "$replay/before-$pair.csv"
  "$replay/after" "$replay/counts.txt" > "$replay/after-$pair.csv"
done
