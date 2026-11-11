"""Read-only fitter witnesses: reported cusp LL versus exact binary coefficients."""
import json,subprocess,sys
from pathlib import Path
import mpmath as mp
H=Path(__file__).resolve().parent;ROOT=H.parents[3];S=ROOT/'build/scratch/dpqr-extreme';S.mkdir(parents=True,exist_ok=True)
sys.path.insert(0,str(H));import audit
x=[-3.,-1.,-.2,0.,.3,.4,2.]
source='''#include "cppunivariate/distributions/sged/mle.hpp"
#include "cppunivariate/distributions/sged/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main(){std::vector<double>x{-3,-1,-.2,0,.3,.4,2};std::cout<<std::setprecision(17);
for(double nu:{.3,.5,.9})for(double xi:{.3,1.3,3.}){auto r=cppunivariate::sged_mle({x.data(),x.size()},1e-8,200,.1,NAN,nu,xi);
double d=0;for(double v:x)d+=cppunivariate::distributions::dsged(v,.1,r.sigma,nu,xi,true);
std::cout<<nu<<" "<<xi<<" "<<r.sigma<<" "<<r.logLik<<" "<<d<<" "<<int(r.status)<<" "<<r.certified<<"\\n";}}
''';(S/'cusp-probe.cpp').write_text(source)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
subprocess.run(['c++','-std=c++17','-O1','-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,str(S/'cusp-probe.cpp'),'-o',str(S/'cusp-probe')],check=True)
lines=subprocess.check_output([str(S/'cusp-probe')],text=True).splitlines();mp.mp.dps=300;rows=[]
for line in lines:
 nu,xi,sd,ll,d,status,cert=map(float,line.split());n,t,s,m=map(mp.mpf,(nu,xi,sd,.1));moment=mp.exp(mp.loggamma(2/n)-(mp.loggamma(1/n)+mp.loggamma(3/n))/2);shift=moment*(t-1/t);sigma=mp.sqrt((1-moment*moment)*(t*t+1/(t*t))+2*moment*moment-1);ls=(mp.loggamma(1/n)-mp.loggamma(3/n))/2;ref=mp.mpf(0)
 for v in x:
  z=(mp.mpf(v)-m)/s*sigma+shift;w=z/(t if z>=0 else 1/t)
  ref+=mp.log(2/(t+1/t))-mp.log(2)-ls-mp.loggamma(1+1/n)-mp.exp(n*(mp.log(abs(w))-ls))-mp.log(s)+mp.log(sigma)
 ref=float(ref);rows.append(dict(data=x,parameters=[.1,sd,nu,xi],reported=ll,dpqr=d,reference=ref,status=int(status),certified=bool(cert),fitter_error=audit.error(ll,ref),dpqr_error=audit.error(d,ref)))
(H/'sged-cusp-witnesses.json').write_text(json.dumps(rows,indent=2)+'\n');print('fresh SGED cusp fits',len(rows),'fitter mismatches',sum(r['fitter_error']>1e-10 for r in rows),'dpqr mismatches',sum(r['dpqr_error']>1e-10 for r in rows),flush=True)
