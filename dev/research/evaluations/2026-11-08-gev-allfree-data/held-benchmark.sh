#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
python3 - <<'PY'
from pathlib import Path
import subprocess,re
root=Path.cwd();base=root/'build/scratch/held-base';base.mkdir(parents=True,exist_ok=True)
for family in ['gev','frechet']:
    source=f'univariateML-cpp/include/cppunivariate/distributions/{family}/mle.hpp'
    original=subprocess.check_output(['git','show','ddb399172bd99091c8d1b04dbf7fe329c8adc43e:'+source],text=True)
    def include(m):
        target=(root/Path(source).parent/m[1]).resolve()
        if family=='frechet' and m[1]=='../gev/mle.hpp':target=base/'gev.hpp'
        return '#include "'+str(target)+'"'
    (base/(family+'.hpp')).write_text(re.sub(r'#include "([^"]+)"',include,original))
p=Path('dev/research/evaluations/2026-11-08-gev-allfree-data/held-benchmark.cpp')
s=p.read_text().replace('cppunivariate/distributions/frechet/mle.hpp',str(Path('build/scratch/held-base/frechet.hpp').resolve()))
Path('build/scratch/held-base/held.cpp').write_text(s)
PY
c++ -O2 -std=c++17 -IunivariateML-cpp/include build/scratch/held-base/held.cpp -o build/scratch/held-before
c++ -O2 -std=c++17 -IunivariateML-cpp/include dev/research/evaluations/2026-11-08-gev-allfree-data/held-benchmark.cpp -o build/scratch/held-after
build/scratch/held-before > dev/research/evaluations/2026-11-08-gev-allfree-data/held-before.txt
build/scratch/held-after > dev/research/evaluations/2026-11-08-gev-allfree-data/held-after.txt
