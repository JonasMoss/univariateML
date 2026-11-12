"""Rebuild af824a98's fitter and replay all archived bnbinom rows plus golden.
Run under /tmp/umvl-cpu/run; source snapshots and executables stay in build/.
"""
import gzip,json,subprocess,re,math
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
BASE='af824a98d78f2f2325c7a675c20c84c45e6bb826'
FAMILY=ROOT/'univariateML-cpp/include/cppunivariate/distributions/bnbinom'
WORK=ROOT/'build/scratch/bnb-loglik';WORK.mkdir(parents=True,exist_ok=True)
# Only mle.hpp is reconstructed: reject changes to its shared dependencies.
changed=subprocess.check_output(['git','diff','--name-only',BASE,'--','univariateML-cpp/include'],cwd=ROOT,text=True).splitlines()
assert all(not p.endswith(('.hpp','.h')) or p=='univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp' for p in changed),changed
cases=[c for c in json.load(gzip.open(ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz','rt')) if c['family']=='bnbinom']
g=json.loads((FAMILY/'golden.json').read_text());cases.append(dict(index='golden',held={},data=g['data']))
for mask in range(1,8):
 cases.append(dict(index=f'golden-mask-{mask}',held={k:g['estimates'][k] for j,k in enumerate(['size','alpha','beta']) if mask&(1<<j)},data=g['data']))
baseline=subprocess.check_output(['git','show',f'{BASE}:univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp'],cwd=ROOT,text=True)
baseline=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((FAMILY/m[1]).resolve())+'"',baseline)
(WORK/'baseline.hpp').write_text(baseline)
source='''#include HEADER
#include <iostream>
#include <iomanip>
int main(){int n;double r,a,b;while(std::cin>>n>>r>>a>>b){std::vector<int>x(n);for(int&v:x)std::cin>>v;auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-10,600,r<0?NAN:r,a<0?NAN:a,b<0?NAN:b);std::cout<<std::setprecision(17)<<static_cast<int>(f.status)<<" "<<f.size<<" "<<f.alpha<<" "<<f.beta<<" "<<f.logLik<<" "<<f.iterations<<" "<<f.certified<<"\\n";}}
'''
(WORK/'runner.cpp').write_text(source)
stdin=''.join(' '.join(map(str,[len(c['data']),*[c['held'].get(k,-1) for k in ['size','alpha','beta']],*c['data']]))+'\n' for c in cases)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
summary={}
mp.mp.dps=100
for mode in ['native','double']:
 results={}
 for label,header in [('before',WORK/'baseline.hpp'),('after',FAMILY/'mle.hpp')]:
  exe=WORK/f'{label}-{mode}'
  subprocess.run(['g++','-std=c++17','-O2','-DNDEBUG','-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,*(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if mode=='double' else []),f'-DHEADER="{header}"',str(WORK/'runner.cpp'),'-o',str(exe)],check=True)
  proc=subprocess.run([str(exe)],input=stdin,text=True,capture_output=True,check=True)
  records=[]
  for c,line in zip(cases,proc.stdout.splitlines(),strict=True):
   v=line.split();records.append(dict(index=c['index'],status=int(v[0]),coefficients=v[1:4],logLik=v[4],iterations=int(v[5]),certified=int(v[6])))
  results[label]=records
  (HERE/f'{label}-{mode}.json').write_text(json.dumps(records,indent=2)+'\n')
  print(mode,label,len(records),flush=True)
 changes=[]
 for c,old,new in zip(cases,results['before'],results['after']):
  assert {k:v for k,v in old.items() if k!='logLik'}=={k:v for k,v in new.items() if k!='logLik'},(mode,c['index'],old,new)
  if old['logLik']==new['logLik']:continue
  assert new['status']==0
  r,a,b=[mp.mpf(float(v)) for v in new['coefficients']]
  ref=sum(mp.loggamma(r+x)-mp.loggamma(r)-mp.loggamma(x+1)+mp.loggamma(r+a)+mp.loggamma(x+b)-mp.loggamma(r+a+x+b)-mp.loggamma(a)-mp.loggamma(b)+mp.loggamma(a+b) for x in c['data'])
  errors={label:float(abs(mp.mpf(float(v['logLik']))-ref)/max(1,abs(ref))) for label,v in [('before',old),('after',new)]}
  assert errors['after']<=1e-10,(mode,c['index'],errors)
  changes.append(dict(index=c['index'],before=old['logLik'],after=new['logLik'],reference=mp.nstr(ref,90),normalized_error=errors))
 summary[mode]=dict(rows=len(cases),changes=changes,coefficients_status_iterations_certified_identical=True)
(HERE/'comparison.json').write_text(json.dumps(summary,indent=2)+'\n')
print({m:len(v['changes']) for m,v in summary.items()},flush=True)
