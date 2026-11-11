"""600-digit independent checks of both words and their propagated moment margin."""
import json,math,subprocess,sys
from pathlib import Path
import mpmath as mp
H=Path(__file__).resolve().parent;ROOT=H.parents[3];S=ROOT/'build/scratch/dpqr-extreme';S.mkdir(parents=True,exist_ok=True)
mp.mp.dps=600
cases=[('ged',v) for v in (.001,.01,.1,.3,1.,2.,3.8792797496374307,5.,30.,1e3,1e8,1e30,1e308)]
cases += [('student',v) for v in (math.nextafter(2.,math.inf),2.00000001,2.5492086449176115,3.093978716511268,5.,30.,1e8,1e30,1e308)]
source='#include "cppunivariate/special/skew_moments.hpp"\n#include <iostream>\n#include <iomanip>\nint main(){std::cout<<std::setprecision(17);\n'
for ident,v in cases:source+='{auto m=cppunivariate::special::skew_moments::'+ident+'('+repr(v)+');std::cout<<m.value.high<<" "<<m.value.low<<" "<<m.error<<" "<<m.valid<<"\\n";}\n'
source+='}\n';(S/'moment-probe.cpp').write_text(source)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
subprocess.run(['c++','-std=c++17','-O1','-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,str(S/'moment-probe.cpp'),'-o',str(S/'moment-probe')],check=True)
lines=subprocess.check_output([str(S/'moment-probe')],text=True).splitlines();rows=[];fail=[]
for (ident,v),line in zip(cases,lines):
 hi,lo,error,valid=map(float,line.split());nu=mp.mpf(v)
 ref=mp.exp(mp.loggamma(2/nu)-mp.loggamma(1/nu)/2-mp.loggamma(3/nu)/2) if ident=='ged' else mp.sqrt(nu-2)/mp.sqrt(mp.pi)*mp.exp(mp.loggamma((nu-1)/2)-mp.loggamma(nu/2))
 err=abs(mp.mpf(hi)+mp.mpf(lo)-ref);r=dict(family=ident,shape=v,high=hi,low=lo,reference=str(ref),actual_error=str(err),margin=error,valid=bool(valid));rows.append(r)
 if valid and err>mp.mpf(error)+mp.mpf(64)*mp.mpf(2.)**-104:fail.append(r)
(H/'skew-moments.json').write_text(json.dumps(dict(checks=len(rows),failures=fail,rows=rows),indent=2)+'\n')
print('skew moments',len(rows),'checks;',len(fail),'failures;',sum(r['valid'] for r in rows),'valid moments',flush=True)
if fail:print(fail[:3]);raise SystemExit(1)
