#!/usr/bin/env python3
"""Compare overlapping tail paths against mpmath; run with CPU slot lock."""
from pathlib import Path
import json,math,subprocess
import mpmath as mp
ROOT=Path(__file__).resolve().parents[2];OUT=ROOT/'build/scratch/step7-tails';OUT.mkdir(parents=True,exist_ok=True)
source=r'''
#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/ged/tails.hpp"
#include "cppunivariate/distributions/foldnorm/dpqr.hpp"
#include "cppunivariate/distributions/emg/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);
for(double a:{.01,.1,1.,10.,100.})for(double lx:{-1000.,-100.,-36.,-10.,-1.,0.,1.,3.,5.,10.})for(bool lower:{false,true}) {
const auto t=cppunivariate::special::gamma_log_tails(lx,a);
std::cout<<"g,"<<a<<","<<lx<<","<<lower<<","<<cppunivariate::ged_detail::gamma_log(lx,a,lower)<<","<<(lower?t.lower:t.upper)<<"\n";
}
for(double z:{0.,1.,7.999,8.,11.999,12.,25.999,26.,37.,100.,1000.,1e150})
std::cout<<"n,"<<z<<","<<cppunivariate::special::normal_log_upper(z)<<","<<cppunivariate::foldnorm_detail::logupper(z)<<","<<cppunivariate::special::fs_normal_p(z,false,true)<<","<<static_cast<double>(cppunivariate::emg_detail::logPhi(-z))<<"\n";
}
'''
(OUT/'probe.cpp').write_text(source)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
subprocess.run(['g++','-std=c++17','-O3','-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,str(OUT/'probe.cpp'),'-o',str(OUT/'probe')],check=True)
raw=subprocess.check_output([str(OUT/'probe')],text=True);(OUT/'raw.csv').write_text(raw)
mp.mp.dps=100
rows=[]
for line in raw.splitlines():
 parts=line.split(',')
 if parts[0]=='g':
  a,lx,lower,local,shared=map(float,parts[1:]);x=mp.exp(mp.mpf(lx));aa=mp.mpf(a)
  ref=mp.log(mp.gammainc(aa,0,x)/mp.gamma(aa)) if lower else mp.log(mp.gammainc(aa,x,mp.inf)/mp.gamma(aa))
  rows.append(dict(kind='gamma',a=a,lx=lx,lower=lower,reference=float(ref),local=local,shared=shared,
   local_error=float(abs(mp.mpf(local)-ref)) if math.isfinite(local) else None,shared_error=float(abs(mp.mpf(shared)-ref)) if math.isfinite(shared) else None))
 else:
  z,*values=map(float,parts[1:]);ref=mp.log(mp.erfc(mp.mpf(z)/mp.sqrt(2))/2)
  rows.append(dict(kind='normal',z=z,reference=float(ref),**{key:val for key,val in zip(['shared','foldnorm','fs','emg'],values)},
   **{key+'_scaled_error':float(abs(mp.mpf(val)-ref)/max(1,abs(ref))) if math.isfinite(val) else None for key,val in zip(['shared','foldnorm','fs','emg'],values)}))
summary={'gamma_cases':100,'gamma_byte_equal':sum(r['local']==r['shared'] for r in rows if r['kind']=='gamma'),
 'gamma_local_nonfinite':sum(r['local_error'] is None for r in rows if r['kind']=='gamma'),
 'gamma_shared_nonfinite':sum(r['shared_error'] is None for r in rows if r['kind']=='gamma')}
for key in ['local','shared']:
 summary['gamma_'+key+'_worst']=max((r for r in rows if r['kind']=='gamma' and r[key+'_error'] is not None),key=lambda r:r[key+'_error'])
summary['normal_max_scaled_error']={key:max(r[key+'_scaled_error'] for r in rows if r['kind']=='normal' and r[key+'_scaled_error'] is not None) for key in ['shared','foldnorm','fs','emg']}
summary['rows']=rows
# Strict JSON retains failed probe values as strings.
def clean(v):
 if isinstance(v,float) and not math.isfinite(v):return str(v)
 if isinstance(v,list):return [clean(x) for x in v]
 if isinstance(v,dict):return {k:clean(x) for k,x in v.items()}
 return v
summary=clean(summary)
(OUT/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps({k:v for k,v in summary.items() if k!='rows'},indent=2))
