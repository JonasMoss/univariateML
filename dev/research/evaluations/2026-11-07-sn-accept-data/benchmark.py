"""Run under /tmp/umvl-cpu/run; build both revisions without changing Git."""
import hashlib, json, pathlib, re, subprocess, statistics
repo=pathlib.Path.cwd()
folder=pathlib.Path(__file__).resolve().parent
scratch=repo/'build/scratch/sn-benchmark'
scratch.mkdir(parents=True,exist_ok=True)
dossier=repo/'univariateML-cpp/include/cppunivariate/distributions/sn'
base='3de415f0896527001caeb0d612bcb91b858d644d'
old=subprocess.check_output(['git','show',base+':univariateML-cpp/include/cppunivariate/distributions/sn/mle.hpp'],text=True)
def include(m):
    path=m.group(1)
    if path.startswith('../') or path=='dpqr.hpp': return '#include "'+str((dossier/path).resolve())+'"'
    return m.group(0)
(scratch/'before.hpp').write_text(re.sub(r'#include "([^"]+)"',include,old))
g=json.loads((dossier/'golden.json').read_text())
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
results={}
for version in ['before','after']:
    header=scratch/'before.hpp' if version=='before' else dossier/'mle.hpp'
    source='''#include "HEADER"
#include <chrono>
#include <ctime>
#include <iomanip>
#include <iostream>
int main(){std::cout<<std::setprecision(17);std::vector<double>x{DATA};
for(int trial=0;trial<1;++trial)for(int mask=0;mask<8;++mask){auto start=std::chrono::steady_clock::now();auto cpu_start=std::clock();
auto f=cppunivariate::sn_mle({x.data(),x.size()},1e-9,100,mask&1?XI:NAN,mask&2?OMEGA:NAN,mask&4?ALPHA:NAN);
const double cpu=double(std::clock()-cpu_start)/CLOCKS_PER_SEC;const double wall=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
std::cout<<trial<<" "<<mask<<" "<<int(f.status)<<" "<<f.xi<<" "<<f.omega<<" "<<f.alpha<<" "<<f.logLik<<" "<<wall<<" "<<cpu<<"\\n";}}
'''.replace('HEADER',str(header)).replace('DATA',','.join(map(repr,g['data'])))
    for key in ['xi','omega','alpha']:source=source.replace(key.upper(),repr(g['estimates'][key]))
    cpp=scratch/(version+'.cpp');exe=scratch/version
    cpp.write_text(source)
    subprocess.run(['c++','-O2','-std=c++17','-I'+str(repo/'univariateML-cpp/include'),'-I'+boost,str(cpp),'-o',str(exe)],check=True)
raw={version:[] for version in ['before','after']}
# Compile both first; alternate execution order to reduce shared-load bias.
for trial in range(5):
    for version in (['before','after'] if trial%2==0 else ['after','before']):
        rows=[list(map(float,r.split())) for r in subprocess.check_output([str(scratch/version)],text=True).splitlines()]
        for row in rows:row[0]=trial
        assert all(row[2]==0 for row in rows),rows
        raw[version].extend(rows)
for version,rows in raw.items():
    results[version]=[dict(mask=m,status=int(next(r[2] for r in rows if r[1]==m)),coef=next(r[3:6] for r in rows if r[1]==m),logLik=next(r[6] for r in rows if r[1]==m),median_seconds=statistics.median(r[7] for r in rows if r[1]==m),median_cpu_seconds=statistics.median(r[8] for r in rows if r[1]==m)) for m in range(8)]
for a,b in zip(results['before'],results['after']):
    assert a['status']==b['status']==0,(a,b)
    assert all(abs(u-v)<=1e-6*max(1,abs(u)) for u,v in zip(a['coef'],b['coef'])),(a,b)
    assert abs(a['logLik']-b['logLik'])<1e-8
    print(b['mask'], 'before/after CPU ms',1000*a['median_cpu_seconds'],1000*b['median_cpu_seconds'],'wall ms',1000*a['median_seconds'],1000*b['median_seconds'],'max coefficient change',max(abs(u-v) for u,v in zip(a['coef'],b['coef'])))
(folder/'benchmark.json').write_text(json.dumps(dict(base_revision=base,after_header_sha256=hashlib.sha256((dossier/'mle.hpp').read_bytes()).hexdigest(),raw_rows=raw,raw_columns=['trial','held_mask','status','xi','omega','alpha','logLik','wall_seconds','cpu_seconds'],sample='unchanged sn golden; hold values at golden estimates; five trials per mask',compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],results=results),indent=2)+'\n')
