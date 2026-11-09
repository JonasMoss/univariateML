#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/allfree-base
python3 - <<'PY'
import pathlib,re,subprocess,json
root=pathlib.Path.cwd();base=root/'build/scratch/allfree-base'
for family in ['gev','frechet']:
    p=f'univariateML-cpp/include/cppunivariate/distributions/{family}/mle.hpp'
    s=subprocess.check_output(['git','show','ddb399172bd99091c8d1b04dbf7fe329c8adc43e:'+p],text=True)
    def inc(m):
        target=(root/pathlib.Path(p).parent/m[1]).resolve()
        if family=='frechet' and m[1]=='../gev/mle.hpp':target=base/'gev.hpp'
        return '#include "'+str(target)+'"'
    (base/(family+'.hpp')).write_text(re.sub(r'#include "([^"]+)"',inc,s))
g=[]
for family in ['gev','frechet']:
    data=json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
    g.append('const std::vector<double> '+family+'_golden{'+','.join(map(repr,data['data']))+'};')
    if family=='gev':g.append('const double gev_mu='+repr(data['estimates']['mu'])+',gev_sigma='+repr(data['estimates']['sigma'])+';')
(root/'build/scratch/evt-joint-golden.hpp').write_text('\n'.join(g))
s=(root/'dev/research/evaluations/2026-11-07-evt-joint-data/benchmark.cpp').read_text()
(base/'bench.cpp').write_text(s.replace('cppunivariate/distributions/frechet/mle.hpp',str(base/'frechet.hpp')))
PY
c++ -O2 -std=c++17 -IunivariateML-cpp/include -Ibuild/scratch build/scratch/allfree-base/bench.cpp -o build/scratch/allfree-before
c++ -O2 -std=c++17 -IunivariateML-cpp/include -Ibuild/scratch dev/research/evaluations/2026-11-07-evt-joint-data/benchmark.cpp -o build/scratch/allfree-after
c++ -O2 -std=c++17 -IunivariateML-cpp/include -Ibuild/scratch dev/research/evaluations/2026-11-08-gev-allfree-data/profile-probe.cpp -o build/scratch/profile-probe
build/scratch/profile-probe > dev/research/evaluations/2026-11-08-gev-allfree-data/profile-slices.txt
for j in 1 2 3; do
 build/scratch/allfree-before > "dev/research/evaluations/2026-11-08-gev-allfree-data/golden-before-$j.txt"
 build/scratch/allfree-after > "dev/research/evaluations/2026-11-08-gev-allfree-data/golden-after-$j.txt"
done
c++ -O2 -std=c++17 -IunivariateML-cpp/include dev/research/evaluations/2026-11-08-gev-allfree-data/two-point-benchmark.cpp -o build/scratch/after
build/scratch/after > dev/research/evaluations/2026-11-08-gev-allfree-data/after.txt
