"""Isolated final arithmetic check; does not mutate replay/build targets."""
from pathlib import Path
import subprocess,json,mpmath as m
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=110
cases=[]
for i,r in enumerate(json.loads((P/'weak-hold-references.json').read_text())):cases.append(dict(id='weak-hold-'+str(i),kind=0,data=r['data'],free=r['free'],inverse=r['inverse'],reference=r['reference']))
inputs={str(c['index']):c for c in json.loads((P/'cases.json').read_text())}
for r in json.loads((P/'probe3-native-references.json').read_text()):
 c=inputs.get(r['id']);ref=r.get('reference',{})
 if c and c['family']=='betapr' and not c['held'] and ref and max(float(ref['shape1']),float(ref['shape2']))<32 and min(float(ref['shape1']),float(ref['shape2']))>0:
  cases.append(dict(id='small-shape-'+r['id'],kind=1,data=c['data'],free=[True]*3,inverse=False,reference=ref))
 if len(cases)>=5:break
assert len(cases)>=4
source=root/'build/scratch/final-numeric.cpp';source.write_text('''#include <cppunivariate/distributions/betapr/mle.hpp>
#include <iostream>
#include <iomanip>
int main(){int kind,n,i0,i1,i2,inv;double a,b,s;while(std::cin>>kind>>n>>a>>b>>s>>i0>>i1>>i2>>inv){std::vector<double>x(n);for(auto&v:x)std::cin>>v;std::array<bool,3>free{{bool(i0),bool(i1),bool(i2)}};bool ok=false;if(kind==0)ok=cppunivariate::burr_local_accuracy({x.data(),x.size()},a,b,s,free,1e-10,1000,inv);else{cppunivariate::BetaPrimeResult r{a,b,s,0,0,cppunivariate::BetaPrimeStatus::Ok};r.certified=false;r=cppunivariate::betapr_detail::accurate_fallback(r,{x.data(),x.size()},i0?NAN:a,i1?NAN:b,i2?NAN:s,1e-10,1000);ok=r.status==cppunivariate::BetaPrimeStatus::Ok;a=r.alpha;b=r.beta;s=r.scale;}std::cout<<std::setprecision(17)<<ok<<" "<<a<<" "<<b<<" "<<s<<"\\n";}}
''')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip();result={}
for mode in ['native','double']:
 exe=root/('build/scratch/final-numeric-'+mode);cmd=['g++','-O2','-std=c++17','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(source),'-o',str(exe)]
 if mode=='double':cmd.insert(1,'-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS')
 subprocess.run(cmd,check=True);lines=[]
 for c in cases:
  values=[float(c['reference'][k])*(1.000001 if c['free'][i] else 1) for i,k in enumerate(['shape1','shape2','scale'])];lines.append(' '.join(map(str,[c['kind'],len(c['data']),*values,*map(int,c['free']),int(c['inverse']),*c['data']])))
 output=subprocess.check_output([str(exe)],input='\n'.join(lines)+'\n',text=True).splitlines();rows=[]
 for c,line in zip(cases,output):
  tokens=line.split();err={k:float(abs(m.mpf(float(v))-m.mpf(c['reference'][k]))/abs(m.mpf(c['reference'][k]))) for k,v in zip(['shape1','shape2','scale'],tokens[1:])};rows.append(dict(id=c['id'],success=tokens[0]=='1',coefficient_errors=err,max_error=max(err.values()),coefficients=tokens[1:]));print(mode,rows[-1],flush=True)
 result[mode]=rows;(P/'final-numeric-probe.json').write_text(json.dumps(result,indent=2)+'\n')
 assert len(rows)==len(cases) and all(r['success'] and r['max_error']<=1e-10 for r in rows)
