#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
python3 - <<'PY'
import pathlib,re,subprocess,json
root=pathlib.Path.cwd();base=root/'build/scratch/evt-joint-baseline';base.mkdir(parents=True,exist_ok=True)
for family in ['gev','frechet']:
    p=f'univariateML-cpp/include/cppunivariate/distributions/{family}/mle.hpp'
    s=subprocess.check_output(['git','show','16067f5ee0f283952daa948d80b91694e2608dd6:'+p],text=True)
    def inc(m):
        target=(root/pathlib.Path(p).parent/m[1]).resolve()
        if family=='frechet' and m[1]=='../gev/mle.hpp':target=base/'gev.hpp'
        return '#include "'+str(target)+'"'
    (base/(family+'.hpp')).write_text(re.sub(r'#include "([^"]+)"',inc,s))
g=[]
for family in ['gev','frechet']:
    data=json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
    g.append('const std::vector<double> '+family+'_golden{'+','.join(map(repr,data['data']))+'};')
    if family=='gev':
        g.append('const double gev_mu='+repr(data['estimates']['mu'])+',gev_sigma='+repr(data['estimates']['sigma'])+';')
(root/'build/scratch/evt-joint-golden.hpp').write_text('\n'.join(g))
s=(root/'dev/research/evaluations/2026-11-07-evt-joint-data/benchmark.cpp').read_text()
(base/'bench.cpp').write_text(s.replace('cppunivariate/distributions/frechet/mle.hpp',str(base/'frechet.hpp')))
PY
c++ -O2 -std=c++17 -IunivariateML-cpp/include -Ibuild/scratch build/scratch/evt-joint-baseline/bench.cpp -o build/scratch/evt-joint-before
c++ -O2 -std=c++17 -IunivariateML-cpp/include -Ibuild/scratch dev/research/evaluations/2026-11-07-evt-joint-data/benchmark.cpp -o build/scratch/evt-joint-after
for j in 1 2 3; do
  build/scratch/evt-joint-before > "dev/research/evaluations/2026-11-07-evt-joint-data/before-$j.txt"
  build/scratch/evt-joint-after > "dev/research/evaluations/2026-11-07-evt-joint-data/after-$j.txt"
done
python3 dev/research/evaluations/2026-11-07-evt-joint-data/check-pair-bound.py > dev/research/evaluations/2026-11-07-evt-joint-data/pair-check.json
c++ -O2 -std=c++17 -IunivariateML-cpp/include -Ibuild/scratch dev/research/evaluations/2026-11-07-evt-joint-data/interval-probe.cpp -o build/scratch/evt-joint-intervals
build/scratch/evt-joint-intervals > dev/research/evaluations/2026-11-07-evt-joint-data/interval-bounds.txt
python3 dev/research/evaluations/2026-11-07-evt-joint-data/check-intervals.py > dev/research/evaluations/2026-11-07-evt-joint-data/interval-check.json
python3 dev/research/evaluations/2026-11-07-evt-joint-data/summarize-benchmark.py > dev/research/evaluations/2026-11-07-evt-joint-data/timing-summary.json
