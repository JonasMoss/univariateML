#!/usr/bin/env bash
# Run from the worktree root. Every computing step reserves one shared slot.
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
d=dev/research/evaluations/2026-11-10-gev-held2-data
b=$(Rscript -e 'cat(system.file("include",package="BH"))')
include=univariateML-cpp/include
phase=${1:-after}
if [ "$phase" = before ]; then
  /tmp/umvl-cpu/run python3 - <<'PY'
import shutil,zipfile
from pathlib import Path
p=Path('build/scratch/held2-before-include')
shutil.copytree('univariateML-cpp/include',p,dirs_exist_ok=True)
with zipfile.ZipFile('dev/research/evaluations/2026-11-10-gev-held2-data/before-headers.zip') as z:
    z.extractall(p)
PY
  include=build/scratch/held2-before-include
fi
for name in study allfree; do
  /tmp/umvl-cpu/run g++ -std=c++17 -O2 -I"$include" -I"$b" "$d/$name.cpp" -o "build/scratch/held2-$phase-$name"
  if [ "$phase" = before ]; then
    label=$name; [ "$name" != study ] || label=held
    /tmp/umvl-cpu/run "build/scratch/held2-$phase-$name" 100 > "$d/$label-before.csv"
  else
    for maxit in 100 1000; do
      /tmp/umvl-cpu/run "build/scratch/held2-$phase-$name" "$maxit" > "$d/$name-local-$maxit.csv"
    done
  fi
done
/tmp/umvl-cpu/run python3 "$d/summarize.py"
