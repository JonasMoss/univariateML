#!/usr/bin/env bash
# This entire script computes sequentially in one reserved slot; no nested locks.
set -euo pipefail
export TMPDIR=$PWD/build/tmp
b=$(Rscript -e 'cat(system.file("include",package="BH"))')
d=dev/research/evaluations/2026-11-10-gev-held2-data
python3 - <<'PY'
import shutil,zipfile
from pathlib import Path
p=Path('build/scratch/held2-before-include')
shutil.copytree('univariateML-cpp/include',p,dirs_exist_ok=True)
with zipfile.ZipFile('dev/research/evaluations/2026-11-10-gev-held2-data/before-headers.zip') as z:z.extractall(p)
PY
for phase in before after; do
  include=univariateML-cpp/include
  [ "$phase" != before ] || include=build/scratch/held2-before-include
  g++ -std=c++17 -O2 -I"$include" -I"$b" "$d/edges.cpp" -o "build/scratch/edges-$phase"
  "build/scratch/edges-$phase" "$phase" > "$d/edges-$phase.csv"
done
