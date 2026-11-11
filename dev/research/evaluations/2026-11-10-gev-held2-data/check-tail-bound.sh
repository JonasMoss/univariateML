#!/usr/bin/env bash
# Run sequentially in one reserved slot; no nested locks.
set -euo pipefail
export TMPDIR=$PWD/build/tmp
b=$(Rscript -e 'cat(system.file("include",package="BH"))')
d=dev/research/evaluations/2026-11-10-gev-held2-data
python3 - <<'PY'
from pathlib import Path
import shutil,zipfile
p=Path('build/scratch/before-tail-include')
shutil.copytree('univariateML-cpp/include',p,dirs_exist_ok=True)
with zipfile.ZipFile('dev/research/evaluations/2026-11-10-gev-held2-data/before-tail-headers.zip') as z:z.extractall(p)
PY
printf 'version,value_upper,mean_log_density,excludes\n'
for phase in before after; do
  include=univariateML-cpp/include
  [ "$phase" != before ] || include=build/scratch/before-tail-include
  g++ -std=c++17 -O2 -I"$include" -I"$b" "$d/tail-bound.cpp" -o "build/scratch/tail-bound-$phase"
  "build/scratch/tail-bound-$phase" "$phase"
done
