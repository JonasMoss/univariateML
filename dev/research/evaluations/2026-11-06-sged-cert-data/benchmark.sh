#!/usr/bin/env bash
# Invoke from repository root through /tmp/umvl-cpu/run.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch
python3 - <<'PY'
from pathlib import Path
import re, subprocess
p=Path('univariateML-cpp/include/cppunivariate/distributions/sged/mle.hpp')
s=subprocess.check_output(['git','show','bb734cd78b846a565d8835606328bb1285a82db6:'+str(p)],text=True)
s=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((p.parent/m[1]).resolve())+'"',s)
Path('build/scratch/sged-before.hpp').write_text(s)
PY
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
source=dev/research/evaluations/2026-11-06-sged-cert-data/benchmark.cpp
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" '-DSGED_BASELINE_HEADER="../../../../build/scratch/sged-before.hpp"' "$source" -o build/scratch/sged-before
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$source" -o build/scratch/sged-after
build/scratch/sged-before > dev/research/evaluations/2026-11-06-sged-cert-data/timings-before.txt
build/scratch/sged-after > dev/research/evaluations/2026-11-06-sged-cert-data/timings-after.txt
