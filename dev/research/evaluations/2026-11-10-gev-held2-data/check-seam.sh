#!/usr/bin/env bash
# One reserved slot, sequential computation, no nested slot wrappers.
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
  g++ -std=c++17 -O2 -I"$include" -I"$b" "$d/seam-fallback.cpp" -o "build/scratch/seam-timed-$phase"
  for maxit in 100 1000; do
    label="$phase-$maxit"
    if [ "$maxit" = 100 ]; then
      label=$phase
      [ "$phase" != after ] || label=after-rescue
    fi
    "build/scratch/seam-timed-$phase" "$maxit" > "$d/seam-$label.csv"
  done
done
