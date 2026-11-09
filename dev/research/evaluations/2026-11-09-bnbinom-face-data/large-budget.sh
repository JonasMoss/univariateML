#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp"
evidence=dev/research/evaluations/2026-11-09-bnbinom-face-data
python3 - <<'PY'
from pathlib import Path
p=Path('dev/research/evaluations/2026-11-09-bnbinom-face-data/probe.cpp')
s=p.read_text().replace('std::vector<int>{0,0,5,5},std::vector<int>{0,2,2,2},std::vector<int>{0,1,2,3,4,5}', 'std::vector<int>{0,1,2,3,4,5}').replace('for(int maxit:{600,6000})','for(int maxit:{100000})')
Path('build/scratch/bnbinom-face/large-budget.cpp').write_text(s)
PY
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -Wall -Wextra -Wpedantic -IunivariateML-cpp/include -I"$bh" build/scratch/bnbinom-face/large-budget.cpp -o build/scratch/bnbinom-face/large-budget
build/scratch/bnbinom-face/large-budget > "$evidence/large-budget.json"
