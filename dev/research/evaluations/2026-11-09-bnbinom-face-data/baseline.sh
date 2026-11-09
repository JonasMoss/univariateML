#!/bin/sh
# Read-only base-header overlay; invoke through run. No checkout or git write.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
evidence=dev/research/evaluations/2026-11-09-bnbinom-face-data
mkdir -p build/scratch/bnbinom-face/baseline/cppunivariate/distributions/bnbinom build/scratch/bnbinom-face/baseline-coverage
cp "$evidence/baseline-mle.hpp" build/scratch/bnbinom-face/baseline/cppunivariate/distributions/bnbinom/mle.hpp
python3 - <<'PY'
from pathlib import Path
s=Path('dev/research/evaluations/2026-11-09-bnbinom-face-data/coverage.cpp').read_text()
s=s.replace('build/scratch/bnbinom-face/coverage/','build/scratch/bnbinom-face/baseline-coverage/')
Path('build/scratch/bnbinom-face/baseline-coverage/coverage.cpp').write_text(s)
PY
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -Wall -Wextra -Wpedantic \
  -Ibuild/scratch/bnbinom-face/baseline -IunivariateML-cpp/include \
  -IunivariateML-cpp/include/cppunivariate/distributions/bnbinom -I"$bh" \
  build/scratch/bnbinom-face/baseline-coverage/coverage.cpp -o build/scratch/bnbinom-face/baseline-coverage/check
build/scratch/bnbinom-face/baseline-coverage/check workstation-base
cp build/scratch/bnbinom-face/baseline-coverage/coverage.csv "$evidence/baseline-workstation.csv"
