#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
# A mirrored simbox tree has no .git; the baseline header is prepared locally.
boost_path=$(Rscript -e 'cat(system.file("include",package="BH"))')
flags=(-std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests -Ibuild/scratch "-I$boost_path" "-DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\"")
python3 - <<'PY'
from pathlib import Path
p=Path('dev/research/evaluations/2026-11-08-betapr-joint-data/baseline-mle.txt')
s=p.read_text().replace('@BETAPR@',str((Path.cwd()/'univariateML-cpp/include/cppunivariate/distributions/betapr').resolve()))
Path('build/scratch/baseline-mle.hpp').write_text(s)
PY
c++ "${flags[@]}" -DBENCH_BASELINE dev/research/evaluations/2026-11-08-betapr-joint-data/random-timing.cpp -o build/scratch/random-before
c++ "${flags[@]}" dev/research/evaluations/2026-11-08-betapr-joint-data/random-timing.cpp -o build/scratch/random-after
build/scratch/random-before > dev/research/evaluations/2026-11-08-betapr-joint-data/random-timing-before.txt
build/scratch/random-after > dev/research/evaluations/2026-11-08-betapr-joint-data/random-timing-after.txt
c++ "${flags[@]}" dev/research/evaluations/2026-11-08-betapr-joint-data/joint-r-diagnostic.cpp -o build/scratch/joint-r-diagnostic
build/scratch/joint-r-diagnostic > dev/research/evaluations/2026-11-08-betapr-joint-data/joint-r-diagnostic.txt
bash dev/research/evaluations/2026-11-08-betapr-joint-data/bounds.sh
python3 - <<'PY'
import json,pathlib
p=pathlib.Path('dev/research/evaluations/2026-11-08-betapr-joint-data')
print('BEGIN_EVIDENCE')
print(json.dumps({f:(p/f).read_text() for f in ['random-timing-before.txt','random-timing-after.txt','joint-r-diagnostic.txt','probe.txt','independent-check.json']}))
PY
