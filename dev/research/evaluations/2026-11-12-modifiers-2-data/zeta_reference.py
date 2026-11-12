"""80-digit finite-sum oracle for widened zeta windows; run under one slot."""
from pathlib import Path
import json,subprocess
import mpmath as mp
mp.mp.dps=80
root=Path.cwd();scratch=root/'build/scratch';source=scratch/'zeta-reference.cpp';exe=scratch/'zeta-reference'
source.write_text(r'''
#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/zeta/truncation.hpp"
#include <iostream>
#include <iomanip>
int main() {
 std::cout<<std::setprecision(17);
 for(int N:{4,100,2000,10000,2147483647})for(double s:{-100.,-2.,0.,.5,1.,2.,100.}) {
  for(int k:{1,N/2,N})std::cout<<N<<" "<<s<<" "<<k<<" "<<cppunivariate::zeta_window_eval('d',k,N,s,true,true)<<" "<<cppunivariate::zeta_window_eval('p',k,N,s,false,true)<<" "<<cppunivariate::zeta_window_eval('p',k,N,s,true,true)<<"\n";
 }
 std::vector<int> x{1,2,3,4,4,4};
 const auto r=cppunivariate::zeta_window_mle({x.data(),x.size()},1,4);
 std::cout<<"fit 4 "<<int(r.status)<<" "<<r.s<<" "<<r.logLik<<"\n";
 std::vector<int> broad{2500,5000,10000};
 const auto wide=cppunivariate::zeta_window_mle({broad.data(),broad.size()},1,10000,NAN,1e-10,400);
 std::cout<<"fit 10000 "<<int(wide.status)<<" "<<wide.s<<" "<<wide.logLik<<"\n";
 std::vector<int> extreme{2147483646,2147483647};
 const auto end=cppunivariate::zeta_window_mle({extreme.data(),extreme.size()},1,2147483647,NAN,1e-10,400);
 std::cout<<"fit 2147483647 "<<int(end.status)<<" "<<end.s<<" "<<end.logLik<<"\n";
 std::vector<int> large{1000000,1000001,1000003,1000008,1000011};
 const auto z=cppunivariate::zeta_window_mle({large.data(),large.size()},1,1000011,NAN,1e-10,400);
 std::cout<<"fit 1000011 "<<int(z.status)<<" "<<z.s<<" "<<z.logLik<<"\n";
}
''')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
subprocess.run(['c++','-std=c++17','-O2','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(source),'-o',str(exe)],check=True)
lines=subprocess.check_output([str(exe)],text=True).splitlines();rows=[]
def power_range(a,b,s):
 if a>b:return mp.mpf(0)
 if s==0:return mp.mpf(b-a+1)
 if s<0 and s==int(s):
  r=int(-s)
  return (mp.bernpoly(r+1,b+1)-mp.bernpoly(r+1,a))/(r+1)
 if s==1:return mp.digamma(b+1)-mp.digamma(a)
 return mp.zeta(s,a)-mp.zeta(s,b+1)
def error(got,expected):
 if mp.isinf(expected):
  assert got==float(expected)
  return 0
 absolute=float(abs(mp.mpf(got)-expected))
 assert absolute<1e-10,(got,expected,absolute)
 if expected and abs(expected)<mp.mpf('1e-6') and float(expected)!=0:
  assert abs(mp.mpf(got)/expected-1)<mp.mpf('1e-10'),(got,expected)
 return absolute
for line in [line for line in lines if not line.startswith("fit ")]:
 N,s,k,ld,ls,lc=map(float,line.split());N=int(N);k=int(k);s=mp.mpf(s)
 den=power_range(1,N,s)
 expected=-s*mp.log(k)-mp.log(den)
 tail=power_range(k+1,N,s)/den;below=power_range(1,k,s)/den
 err=error(ld,expected);terr=error(ls,mp.log(tail));cerr=error(lc,mp.log(below))
 rows.append(dict(N=N,s=float(s),k=k,log_density_error=err,log_survival_error=terr,log_cdf_error=cerr))
fits=[]
for line in [line for line in lines if line.startswith('fit ')]:
 _,N,status,s,ll=line.split();N=int(N);s=float(s)
 assert int(status)==0,(N,status,s,ll)
 x={4:[1,2,3,4,4,4],10000:[2500,5000,10000],2147483647:[2147483646,2147483647],1000011:[1000000,1000001,1000003,1000008,1000011]}[N]
 target=mp.fsum(mp.log(mp.mpf(v)/N) for v in x)/len(x)
 def score(shape):
  first=1 if N<=10000 else N-5000
  weights=[mp.power(mp.mpf(j)/N,-shape) for j in range(first,N+1)]
  den=mp.fsum(weights)
  if first>1:
   ratio=mp.power(mp.mpf(first-1)/first,-shape)
   assert weights[0]*ratio/(1-ratio)<den*mp.mpf('1e-60')
  return mp.fsum(w*mp.log(mp.mpf(j)/N) for j,w in zip(range(first,N+1),weights))/den-target
 ref=mp.findroot(score,(s*.999,s*1.001));error=float(abs(s-ref)/max(1,abs(ref)))
 assert error<1e-10,(N,s,ref,error)
 fits.append(dict(N=N,computed=s,reference=str(ref),scaled_error=error,logLik=float(ll)))
result=dict(precision=80,baseline='fe17e3b8',rows=rows,MLE=fits)
(root/'dev/research/evaluations/2026-11-12-modifiers-2-data/zeta-reference.json').write_text(json.dumps(result,indent=2)+'\n')
print(len(rows),'law comparisons and',len(fits),'MLE comparisons passed')
