set -eu
mkdir -p build/tmp build/scratch/bnbinom-alpha
export TMPDIR="$PWD/build/tmp"
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$bh" dev/research/evaluations/2026-11-07-bnbinom-alpha-data/probe.cpp -o build/scratch/bnbinom-alpha/probe
python3 - <<'PY'
import json
from pathlib import Path
x=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json').read_text())['data']
Path('build/scratch/bnbinom-alpha/counts.txt').write_text('\n'.join(map(str,x)))
PY
build/scratch/bnbinom-alpha/probe
build/scratch/bnbinom-alpha/probe build/scratch/bnbinom-alpha/counts.txt
