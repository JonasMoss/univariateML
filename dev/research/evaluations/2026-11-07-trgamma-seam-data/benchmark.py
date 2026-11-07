"""Paired baseline/current timings on the workstation, under one CPU slot.
Reconstructs only the two baseline headers in ignored scratch; no checkout edits.
"""
import hashlib,json,pathlib,re,subprocess,platform
ROOT=pathlib.Path(__file__).resolve().parents[4];HERE=pathlib.Path(__file__).resolve().parent;BUILD=ROOT/'build/scratch'
BASE='3de415f0896527001caeb0d612bcb91b858d644d'
for family in ['trgamma','invtrgamma']:
    path=pathlib.Path(f'univariateML-cpp/include/cppunivariate/distributions/{family}/mle.hpp')
    s=subprocess.check_output(['git','show',BASE+':'+str(path)],cwd=ROOT,text=True)
    def include(m):
        target=(ROOT/path.parent/m[1]).resolve()
        if family=='invtrgamma' and m[1]=='../trgamma/mle.hpp':target=BUILD/'baseline-trgamma.hpp'
        return '#include "'+str(target)+'"'
    s=re.sub(r'#include "([^"]+)"',include,s)
    (BUILD/f'baseline-{family}.hpp').write_text(s)
bh=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
x=json.loads((ROOT/'univariateML-cpp/include/cppunivariate/distributions/trgamma/golden.json').read_text())['data']
rows={}
for baseline in [True,False]:
    exe=BUILD/('bench-baseline' if baseline else 'bench-current')
    cmd=['c++','-std=c++17','-O2','-I'+str(BUILD),'-I'+str(ROOT/'univariateML-cpp/include'),'-I'+bh]
    if baseline:cmd+=['-DUMVL_BASELINE']
    subprocess.run(cmd+[str(HERE/'benchmark.cpp'),'-o',str(exe)],check=True)
    data=subprocess.check_output([str(exe)],input=' '.join(map(str,x)),text=True)
    parsed=[]
    for line in data.splitlines():
        inv,held,status,a,b,scale,ll,ms=map(float,line.split());assert status==0,line
        parsed.append(dict(family='invtrgamma' if inv else 'trgamma',held_scale=bool(held),coef=[a,b,scale],loglik=ll,ms=ms))
    rows['baseline' if baseline else 'current']=parsed
for before,after in zip(rows['baseline'],rows['current']):
    tolerance=2e-10 if after['family']=='invtrgamma' else 2e-11
    assert max(abs(a/b-1) for a,b in zip(after['coef'],before['coef']))<tolerance,(before,after)
    assert abs(after['loglik']-before['loglik'])<1e-8,(before,after)
rows.update(base_revision=BASE,repetitions=20,n=500,compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],machine=platform.platform(),source_sha256={f:hashlib.sha256((ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{f}/mle.hpp').read_bytes()).hexdigest() for f in ['trgamma','invtrgamma']})
(HERE/'timings.json').write_text(json.dumps(rows,indent=2)+'\n')
print(json.dumps(rows,indent=2))
