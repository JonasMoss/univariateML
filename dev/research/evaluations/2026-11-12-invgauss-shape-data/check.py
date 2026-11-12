#!/usr/bin/env python3
"""Rebuild baseline c4ed89f6 via git show; compare exact binary64 inputs at 160/220 digits.
Run from repository root through /tmp/umvl-cpu/run python3 <this file>.
No fixture or production file is rewritten. Scratch executables stay in build/.
"""
import collections, gzip, hashlib, itertools, json, math, pathlib, subprocess
import mpmath as mp
ROOT=pathlib.Path.cwd(); OUT=ROOT/'dev/research/evaluations/2026-11-12-invgauss-shape-data'
WORK=ROOT/'build/scratch/invgauss-shape'; WORK.mkdir(parents=True,exist_ok=True)
BASE='c4ed89f6'; folder='univariateML-cpp/include/cppunivariate/distributions/invgauss'
baseline=subprocess.check_output(['git','show',BASE+':'+folder+'/mle.hpp'],text=True)
for old,new in [('../../core/precision.hpp','cppunivariate/core/precision.hpp'),('../../core/span.hpp','cppunivariate/core/span.hpp'),('dpqr.hpp','cppunivariate/distributions/invgauss/dpqr.hpp')]:
    baseline=baseline.replace('"'+old+'"','"'+new+'"')
(WORK/'baseline.hpp').write_text(baseline)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
source='''#include <iostream>
#include <iomanip>
#include <vector>
#include <string>
#include <cstdlib>
#include HEADER
int main(){int k,mask;while(std::cin>>k>>mask){std::string a,b;std::cin>>a>>b;
 double mu=std::strtod(a.c_str(),nullptr),shape=std::strtod(b.c_str(),nullptr);
 std::vector<double>x;for(int i=0;i<k;++i){int count;std::string s;std::cin>>s>>count;
 x.insert(x.end(),count,std::strtod(s.c_str(),nullptr));}
 auto r=cppunivariate::invgauss_mle(cppunivariate::span{x.data(),x.size()},mask&1?mu:NAN,mask&2?shape:NAN);
 std::cout<<std::setprecision(17)<<int(r.status)<<" "<<r.mu<<" "<<r.lambda<<" "<<r.logLik<<std::endl;}}
'''
executables={}
for version in ['before','after']:
 for mode in ['native','double']:
    src=WORK/(version+'-'+mode+'.cpp'); exe=src.with_suffix('')
    header=str(WORK/'baseline.hpp') if version=='before' else 'cppunivariate/distributions/invgauss/mle.hpp'
    src.write_text(source.replace('HEADER','"'+header+'"'))
    subprocess.run(['ccache','c++','-O2','-std=c++17','-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,*(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if mode=='double' else []),str(src),'-o',str(exe)],check=True)
    executables[version,mode]=exe
cases=[]
archive=json.loads(gzip.decompress((ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz').read_bytes()))
for c in archive:
 if c['family']=='invgauss':cases.append(dict(name='archive-'+str(c['index']),group='archive',values=[(v,len(list(group))) for v,group in itertools.groupby(c['data'])],held=c['held']))
g=json.loads((ROOT/folder/'golden.json').read_text())
cases.append(dict(name='golden',group='golden',values=[(v,len(list(group))) for v,group in itertools.groupby(g['data'])],held={}))
for n in [2,5,1000,1000000]:
 for scale in [1e-300,1e-150,1.,1e150,1e270,1e300,1e307]:
  for spread in [2**-52,1e-12,1e-6,.25]:
   if scale>=1e300 and spread!=.25:continue # free shape otherwise exceeds binary64
   values=collections.Counter()
   for i in range(min(n,5)):
    count=(n+4-i)//5 if n>=5 else 1
    values[scale*(1+(i-2)*spread)]+=count
   cases.append(dict(name=f'grid-{n}-{scale}-{spread}',group='stress',values=list(values.items()),held={}))
# A held mean can be far outside the data scale without preventing a fit.
cases.append(dict(name='held-mean-scale-gap',group='stress',
 values=[(1e-300,1),(2e-300,1),(4e-300,1)],held={'mean':1e300}))
# Check all routes for every dataset, plus the archive's actual holds.
requests=[]
for c in cases:
 n=sum(k for v,k in c['values']); mean=math.fsum(v*(k/n) for v,k in c['values'])
 for mask in range(4):
  held={'mean':mean,'shape':max(v for v,k in c['values'])}
  held.update(c['held'])
  requests.append(dict(c,mask=mask,held=held))
def reference(c,dps):
 with mp.workdps(dps):
  values=[(mp.mpf(v),k) for v,k in c['values']]; n=sum(k for v,k in values)
  mu=mp.mpf(c['held']['mean']) if c['mask']&1 else mp.fsum(v*k for v,k in values)/n
  # Independent direct formula in arbitrary precision, no scaled reductions.
  q=mp.fsum(k*(v-mu)**2/(mu**2*v) for v,k in values)
  shape=mp.mpf(c['held']['shape']) if c['mask']&2 else n/q if q else mp.inf
  return float(mu),float(shape)
refs=[reference(c,160) for c in requests]
assert refs==[reference(c,220) for c in requests]
lines=[' '.join(map(str,[len(c['values']),c['mask'],c['held']['mean'],c['held']['shape'],*[z for v,k in c['values'] for z in (v,k)]])) for c in requests]
records=[]
for (version,mode),exe in executables.items():
 print('running',version,mode,len(requests),flush=True)
 result=subprocess.run([str(exe)],input='\n'.join(lines)+'\n',text=True,capture_output=True,check=True)
 rows=result.stdout.splitlines(); assert len(rows)==len(requests)
 for c,ref,row in zip(requests,refs,rows):
  status,mu,shape,ll=map(float,row.split()); p=[mu,shape]
  errors=[abs(a/b-1) if math.isfinite(a) else math.inf for i,(a,b) in enumerate(zip(p,ref)) if not(c['mask']&(1<<i)) and math.isfinite(b)]
  records.append(dict(name=c['name'],group=c['group'],mask=c['mask'],version=version,mode=mode,status=int(status),parameters=p,reference=ref,max_relative_error=max(errors,default=0),logLik=ll))
summary=[]
for version,mode in executables:
 for group in ['archive','golden','stress']:
  rr=[r for r in records if (r['version'],r['mode'],r['group'])==(version,mode,group)]
  finite=[r for r in rr if all(math.isfinite(v) for v in r['reference'])]
  summary.append(dict(version=version,mode=mode,group=group,requests=len(rr),max_relative_error=max(r['max_relative_error'] for r in finite),coefficient_failures=sum(r['max_relative_error']>1e-10 for r in finite),unexpected_stops=sum(r['status']!=0 for r in finite)))
def finite_json(value):
 if isinstance(value,float) and not math.isfinite(value):return str(value)
 if isinstance(value,dict):return {k:finite_json(v) for k,v in value.items()}
 if isinstance(value,(tuple,list)):return [finite_json(v) for v in value]
 return value
(OUT/'coefficients.json').write_text(json.dumps(finite_json(dict(baseline=BASE,compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],source_sha256=hashlib.sha256((ROOT/folder/'mle.hpp').read_bytes()).hexdigest(),reference_digits=[160,220],datasets=cases,summary=summary,records=records)),indent=2,allow_nan=False)+'\n')
print(json.dumps(summary,indent=2),flush=True)
assert all(r['max_relative_error']<=1e-10 and r['status']==0 for r in records if r['version']=='after' and all(math.isfinite(v) for v in r['reference']))
assert all(all(r['parameters'][i]==r['reference'][i] for i in range(2) if r['mask']&(1<<i))
           for r in records if r['version']=='after')
