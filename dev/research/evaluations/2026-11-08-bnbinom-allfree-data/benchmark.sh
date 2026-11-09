#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp"
evidence=dev/research/evaluations/2026-11-08-bnbinom-allfree-data
mkdir -p build/tmp build/scratch/bnbinom-allfree/base/cppunivariate/distributions/bnbinom
base="$PWD/build/scratch/bnbinom-allfree/base/cppunivariate"
for f in mle.hpp pair_certificate.hpp; do cp "$evidence/baseline/$f" "$base/distributions/bnbinom/$f"; done
for d in core special; do ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/$d" "$base/$d"; done
for d in nbinom pois; do ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/distributions/$d" "$base/distributions/$d"; done
ln -sfn "$PWD/univariateML-cpp/include/cppunivariate/distributions/bnbinom/dpqr.hpp" "$base/distributions/bnbinom/dpqr.hpp"
python3 - <<'PY'
import json,hashlib
from pathlib import Path
folder=Path("dev/research/evaluations/2026-11-08-bnbinom-allfree-data/baseline")
for name,digest in json.loads((folder/"source.json").read_text())["sha256"].items():
    assert hashlib.sha256((folder/name).read_bytes()).hexdigest()==digest
x=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json').read_text())['data']
Path('build/scratch/bnbinom-allfree/counts.txt').write_text('\n'.join(map(str,x)))
PY
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
evidence=dev/research/evaluations/2026-11-08-bnbinom-allfree-data
c++ -O2 -std=c++17 -Ibuild/scratch/bnbinom-allfree/base -IunivariateML-cpp/include -I"$bh" "$evidence/benchmark.cpp" -o build/scratch/bnbinom-allfree/before
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$bh" "$evidence/benchmark.cpp" -o build/scratch/bnbinom-allfree/after
for pair in 1 2 3; do
  build/scratch/bnbinom-allfree/before build/scratch/bnbinom-allfree/counts.txt > "$evidence/before-$pair.csv"
  build/scratch/bnbinom-allfree/after build/scratch/bnbinom-allfree/counts.txt > "$evidence/after-$pair.csv"
done
python3 "$evidence/summarize.py"
python3 "$evidence/audit.py"
