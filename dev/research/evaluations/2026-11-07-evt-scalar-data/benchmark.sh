#!/bin/bash
# Run from the repository root through /tmp/umvl-cpu/run.
set -euo pipefail
python3 - <<'PY'
import pathlib,re,subprocess
root=pathlib.Path.cwd();base=root/'build/scratch/baseline';base.mkdir(parents=True,exist_ok=True)
for family in ['gev','frechet']:
    p=f'univariateML-cpp/include/cppunivariate/distributions/{family}/mle.hpp'
    s=subprocess.check_output(['git','show','7c52f905:'+p],text=True)
    def inc(m):
        target=(root/pathlib.Path(p).parent/m[1]).resolve()
        if family=='frechet' and m[1]=='../gev/mle.hpp':target=base/'gev.hpp'
        return '#include "'+str(target)+'"'
    (base/(family+'.hpp')).write_text(re.sub(r'#include "([^"]+)"',inc,s))
s=(root/'dev/research/evaluations/2026-11-07-evt-scalar-data/benchmark.cpp').read_text()
(base/'bench.cpp').write_text(s.replace('cppunivariate/distributions/frechet/mle.hpp',str(base/'frechet.hpp')))
PY
c++ -O2 -std=c++17 -IunivariateML-cpp/include build/scratch/baseline/bench.cpp -o build/scratch/evt-baseline
build/scratch/evt-baseline > dev/research/evaluations/2026-11-07-evt-scalar-data/before.txt
c++ -O2 -std=c++17 -IunivariateML-cpp/include dev/research/evaluations/2026-11-07-evt-scalar-data/benchmark.cpp -o build/scratch/evt-after
build/scratch/evt-after > dev/research/evaluations/2026-11-07-evt-scalar-data/after.txt
python3 dev/research/evaluations/2026-11-07-evt-scalar-data/check_math.py > dev/research/evaluations/2026-11-07-evt-scalar-data/math-check.json
for i in 1 2 3 4 5; do
  build/scratch/evt-baseline > "dev/research/evaluations/2026-11-07-evt-scalar-data/before-$i.txt"
  build/scratch/evt-after > "dev/research/evaluations/2026-11-07-evt-scalar-data/after-$i.txt"
done
python3 dev/research/evaluations/2026-11-07-evt-scalar-data/timing_summary.py
