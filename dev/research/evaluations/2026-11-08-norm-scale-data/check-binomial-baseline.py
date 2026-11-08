#!/usr/bin/env python3
"""Reproduce the stale JS expectation from untouched HEAD headers in WASM.
Run with the Emscripten environment loaded, under a CPU reservation. No checkout,
index, dependency installation or library source mutation is performed.
"""
import hashlib,json,os,re,subprocess,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
OUT=Path(__file__).resolve().parent
SCRATCH=ROOT/'build/scratch/binomial-baseline'
BASE=SCRATCH/'source'
BASE.mkdir(parents=True,exist_ok=True)
revision='cfb7c4b37d13d1f23ca8ceb546333eb87290b1f6'
seen={}
def extract(path):
    path=Path(path)
    key=path.as_posix()
    if key in seen:return
    data=subprocess.check_output(['git','show',f'{revision}:{key}'],cwd=ROOT)
    seen[key]=hashlib.sha256(data).hexdigest()
    target=BASE/path;target.parent.mkdir(parents=True,exist_ok=True);target.write_bytes(data)
    for name in re.findall(r'^\s*#\s*include\s*"([^"]+)"',data.decode(),re.M):
        child=(Path('univariateML-cpp/include')/name if name.startswith('cppunivariate/') else path.parent/name)
        child=Path(os.path.normpath(child))
        extract(child)
for family in ['ztbinom','zmbinom','zib','tbinom']:
    extract(Path('bindings/common/distributions')/family/'fit.hpp')
source=SCRATCH/'probe.cpp'
source.write_text(r'''
#include "common/distributions/ztbinom/fit.hpp"
#include "common/distributions/zmbinom/fit.hpp"
#include "common/distributions/zib/fit.hpp"
#include "common/distributions/tbinom/fit.hpp"
#include <iostream>
#include <iomanip>
int main() {
  const std::vector<int> a{1,1,1,2,3,7,12},b{0,0,1,1,1,2,3,7,12},c{0,0,0,0,0,0,0,0,1,1,1,2,3,7,12},d{1,2,3};
  const umvl_binding::Held held{true,.2};
  const std::vector<umvl_binding::FitResult> results{
    umvl_binding::fit_ztbinom({a.data(),a.size()}),
    umvl_binding::fit_zmbinom({b.data(),b.size()},{},{},held),
    umvl_binding::fit_zib({c.data(),c.size()},{},{},held),
    umvl_binding::fit_tbinom({d.data(),d.size()})};
  const std::vector<std::string> names{"ztbinom","zmbinom","zib","tbinom"};
  for(std::size_t i=0;i<results.size();++i) {
    const auto& r=results[i];
    std::cout<<std::setprecision(17)<<names[i]<<' '<<r.error_kind<<' '<<r.model<<' '
             <<r.loglik_lower<<' '<<r.loglik_upper<<' '<<r.candidates.size()<<'\n';
  }
}
''')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
exe=SCRATCH/'probe.mjs'
if '--reuse-binary' not in sys.argv: subprocess.run(['em++','-std=c++17','-O2','-I'+str(BASE/'bindings'),'-I'+str(BASE/'univariateML-cpp/include'),'-I'+boost,
                str(source),'-sENVIRONMENT=node','-sEXIT_RUNTIME=1','-o',str(exe)],check=True)
lines=subprocess.check_output(['node','--v8-pool-size=1','--input-type=module','-e',
    'const {default:create}=await import(process.argv[1]);await create();',exe.as_uri()],text=True).splitlines()
rows=[]
for line in lines:
    family,kind,model,lower,upper,count=line.split()
    assert kind=='limit' and float(lower)<=float(upper) and float(upper)-float(lower)<1e-7 and int(count)>=2,line
    rows.append(dict(family=family,kind=kind,model=model,lower=float(lower),upper=float(upper),candidates=int(count)))
assert len(rows)==4
result=dict(revision=revision,all_headers_unchanged=all(hashlib.sha256((ROOT/p).read_bytes()).hexdigest()==h for p,h in seen.items()),headers=seen,results=rows)
assert result['all_headers_unchanged']
(OUT/'binomial-baseline.json').write_text(json.dumps(result,indent=2)+'\n')
print(f'Untouched baseline WASM: all four certified limits; {len(seen)} unchanged headers')
