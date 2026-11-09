#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR=$HOME/.cache/ccache/univariateML CCACHE_BASEDIR=$PWD CCACHE_NOHASHDIR=1
mkdir -p build/tmp build/scratch/ged-perf
files=dev/research/evaluations/2026-11-09-ged-perf-data
python3 "$files/prepare_benchmark.py"
python3 - <<'PY'
import json,pathlib
x=json.loads(pathlib.Path('univariateML-cpp/include/cppunivariate/distributions/ged/golden.json').read_text())['data']
pathlib.Path('build/scratch/ged-perf/data.txt').write_text('\n'.join(map(str,x)))
PY
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
for variant in ${1:-before after}; do
 for kind in time profile; do
  suffix=; flags=()
  if [ "$kind" = profile ]; then suffix=-profile; flags=(-DGED_PERF_PROFILE); fi
  ccache c++ -std=c++17 -O2 "${flags[@]}" -I"build/scratch/ged-perf/$variant$suffix/include" -I"$boost_dir" "$files/benchmark.cpp" -o "build/scratch/ged-perf/$variant-$kind.exe"
  reps=3; if [ "$kind" = profile ]; then reps=1; fi
  "build/scratch/ged-perf/$variant-$kind.exe" "$reps" < build/scratch/ged-perf/data.txt > "$files/$variant-$kind.txt"
 done
 done
