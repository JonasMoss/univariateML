#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/scratch "$TMPDIR"
python3 - <<'PYBASE'
import pathlib,tarfile,re,json
root=pathlib.Path.cwd();base=root/'build/scratch/g1-before';archive=root/'dev/research/evaluations/2026-11-08-gev-allfree-data/pre-g1-headers.tar.gz'
with tarfile.open(archive) as tar:
 members={m.name:tar.extractfile(m).read().decode() for m in tar.getmembers()}
 for path,s in members.items():
  original=root/path;dest=base/original.parent.name/original.name;dest.parent.mkdir(parents=True,exist_ok=True)
  def inc(m):
   target=(original.parent/m[1]).resolve()
   if str(target.relative_to(root)) in members:target=base/target.parent.name/target.name
   return '#include "'+str(target)+'"'
  dest.write_text(re.sub(r'#include "([^"]+)"',inc,s))
 declarations=[]
 for family in ['gev','frechet']:
  g=json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
  declarations.append('const std::vector<double> '+family+'_golden{'+','.join(map(repr,g['data']))+'};')
 (root/'build/scratch/evt-joint-golden.hpp').write_text('\n'.join(declarations))
PYBASE
c++ -std=c++17 -O2 -DBEFORE_G1 -IunivariateML-cpp/include -Ibuild/scratch dev/research/evaluations/2026-11-08-gev-allfree-data/joint-benchmark.cpp -o build/scratch/joint-final-before
c++ -std=c++17 -O2 -IunivariateML-cpp/include -Ibuild/scratch dev/research/evaluations/2026-11-08-gev-allfree-data/joint-benchmark.cpp -o build/scratch/joint-final-after
mkdir -p build/scratch/g1-final-benchmark
build/scratch/joint-final-before > build/scratch/g1-final-benchmark/joint-final-before.jsonl
build/scratch/joint-final-after > build/scratch/g1-final-benchmark/joint-final-after.jsonl
cp build/scratch/g1-final-benchmark/joint-*.jsonl dev/research/evaluations/2026-11-08-gev-allfree-data/
python3 - <<'PY'
import pathlib,json
p=pathlib.Path('dev/research/evaluations/2026-11-08-gev-allfree-data')
before=[json.loads(s) for s in (p/'joint-final-before.jsonl').read_text().splitlines()]
after=[json.loads(s) for s in (p/'joint-final-after.jsonl').read_text().splitlines()]
for b,a in zip(before,after):
 assert a['status']==0,a
 if a['n']==500:
  g=json.loads(pathlib.Path(f"univariateML-cpp/include/cppunivariate/distributions/{a['case']}/golden.json").read_text())
  names=['mu','sigma','xi'] if a['case']=='gev' else ['lambda','mu','sigma']
  for name,value in zip(names,a['coef']):assert abs(value-g['estimates'][name])<=1e-10*abs(g['estimates'][name]),(a,name)
  assert abs(a['logLik']-g['logLik'])<=1e-10*abs(g['logLik'])
 assert a['logLik']>=b['logLik']-1e-8*len(a['coef']), (b,a)
print('Four default fits, frozen golden agreement and likelihood comparisons: passed')
PY
