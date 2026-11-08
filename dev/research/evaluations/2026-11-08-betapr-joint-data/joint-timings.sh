#!/usr/bin/env bash
# Run from the worktree root through /tmp/umvl-cpu/run.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
boost_path=$(Rscript -e 'cat(system.file("include",package="BH"))')
flags=(-std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests -Ibuild/scratch "-I$boost_path" "-DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\"")
python3 - <<'PY'
from pathlib import Path
import re,subprocess
root=Path.cwd();directory=root/'univariateML-cpp/include/cppunivariate/distributions/betapr'
s=subprocess.check_output(['git','show','9fee8c34b838db7b11b7518a071f61495d23f282:univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp'],text=True)
s=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((directory/m[1]).resolve())+'"',s)
(root/'build/scratch/baseline-mle.hpp').write_text(s)
PY
c++ "${flags[@]}" -DBENCH_BASELINE dev/research/evaluations/2026-11-08-betapr-joint-data/joint-timing.cpp -o build/scratch/joint-before
c++ "${flags[@]}" dev/research/evaluations/2026-11-08-betapr-joint-data/joint-timing.cpp -o build/scratch/joint-after
build/scratch/joint-before > dev/research/evaluations/2026-11-08-betapr-joint-data/joint-timing-before.txt
build/scratch/joint-after > dev/research/evaluations/2026-11-08-betapr-joint-data/joint-timing-after.txt
