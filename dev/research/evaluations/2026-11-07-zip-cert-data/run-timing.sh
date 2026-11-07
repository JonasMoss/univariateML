#!/usr/bin/env bash
# Run from repository root through /tmp/umvl-cpu/run.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch
python3 - <<'PY'
from pathlib import Path
import subprocess,re
folder=Path.cwd()/'univariateML-cpp/include/cppunivariate/distributions/zip'
source=subprocess.check_output(['git','show','017b9f8e:univariateML-cpp/include/cppunivariate/distributions/zip/mle.hpp'],text=True)
source=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((folder/m[1]).resolve())+'"',source)
Path('build/scratch/zip-baseline.hpp').write_text(source)
PY
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
evidence=dev/research/evaluations/2026-11-07-zip-cert-data
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$boost" -DZIP_BASELINE_HEADER=\"$PWD/build/scratch/zip-baseline.hpp\" "$evidence/timing.cpp" -o build/scratch/zip-timing-before
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$boost" "$evidence/timing.cpp" -o build/scratch/zip-timing-after
python3 "$evidence/timing-summary.py" build/scratch/zip-timing-before build/scratch/zip-timing-after
