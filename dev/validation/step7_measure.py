#!/usr/bin/env python3
"""Step-7 deterministic numerical probes; run through /tmp/umvl-cpu/run.
Requires existing mpmath, g++, R/BH. Outputs ignored CSV/JSON under build/scratch.
No package installation, fixture generation or repository-external writes.
"""
from pathlib import Path
import csv, json, math, platform, subprocess, sys
import mpmath as mp
ROOT=Path(__file__).resolve().parents[2]
OUT=ROOT/'build/scratch/step7'; OUT.mkdir(parents=True,exist_ok=True)
mp.mp.dps=100
source=r'''
#include "cppunivariate/core/lambert_w.hpp"
#include "cppunivariate/core/inverse_digamma.hpp"
#include "cppunivariate/core/newton.hpp"
#include "cppunivariate/distributions/foldnorm/dpqr.hpp"
#include "cppunivariate/distributions/skellam/dpqr.hpp"
#include "cppunivariate/distributions/pig/dpqr.hpp"
#include "cppunivariate/distributions/snorm/fs.hpp"
#include <cstring>
// Benchmark-only backend comparison, not a library bypass of the seam.
#include <boost/math/special_functions/lambert_w.hpp>
#include <boost/version.hpp>
#include <chrono>
#include <iostream>
#include <iomanip>
#include <vector>
int main() {
 std::cout<<std::setprecision(17);
 std::cout<<"version,"<<BOOST_LIB_VERSION<<"\n";
 int kind; double z;
 while(std::cin>>kind>>z) {
  if(kind<2) {
   double own=kind?cppunivariate::lambertwm1(z):cppunivariate::lambertw0(z);
   double boost=kind?boost::math::lambert_wm1(z,cppunivariate::special::NoThrowPolicy{}):boost::math::lambert_w0(z,cppunivariate::special::NoThrowPolicy{});
   std::cout<<kind<<","<<z<<","<<own<<","<<boost<<"\n";
  } else std::cout<<kind<<","<<z<<","<<cppunivariate::inverse_digamma_scalar(z)<<",0\n";
 }
 volatile double sink=0;
 std::vector<double> zs;
 for(int i=0;i<1000;++i) { double g=1.001+i*.02;zs.push_back(-g*std::exp(-g)); }
 for(int k=0;k<4;++k) {
  auto begin=std::chrono::steady_clock::now();
  for(int j=0;j<200;++j)for(double x:zs) {
   if(k==0)sink+=cppunivariate::lambertw0(x);
   if(k==1)sink+=boost::math::lambert_w0(x,cppunivariate::special::NoThrowPolicy{});
   if(k==2)sink+=cppunivariate::lambertwm1(x);
   if(k==3)sink+=boost::math::lambert_wm1(x,cppunivariate::special::NoThrowPolicy{});
  }
  std::cerr<<"timing_ns,"<<k<<","<<std::chrono::duration<double,std::nano>(std::chrono::steady_clock::now()-begin).count()/200000<<"\n";
 }
 int complement_diff=0, complement_total=0, sum_diff=0, sum_total=0;
 for(int i=1;i<=20000;++i) {
  double x=-std::exp(-700.+i*1400./20000.);
  const double a=cppunivariate::special::log1mexp(x);
  for(double b:{cppunivariate::foldnorm_detail::log1mexp(x),cppunivariate::distributions::fs::log1mexp(x),cppunivariate::special::fs_log1mexp(x)}) {
   ++complement_total;if(std::memcmp(&a,&b,sizeof(double)))++complement_diff;
  }
  double y=-i*.001, z=-i*.003;
  const double c=cppunivariate::special::log_sum_exp(y,z);
  for(double d:{cppunivariate::foldnorm_detail::logadd(y,z),cppunivariate::skellam_detail::logadd(y,z),cppunivariate::pig_detail::logadd(y,z)}) {
   ++sum_total;if(std::memcmp(&c,&d,sizeof(double)))++sum_diff;
  }
 }
 std::cerr<<"tail_complement_byte_differences,"<<complement_diff<<","<<complement_total<<"\n";
 std::cerr<<"tail_sum_byte_differences,"<<sum_diff<<","<<sum_total<<"\n";
 for(double x:{-.6931471805599453,0.,-std::numeric_limits<double>::infinity(),std::numeric_limits<double>::infinity()})
  std::cerr<<"complement_endpoint,"<<x<<","<<cppunivariate::special::log1mexp(x)<<","<<cppunivariate::foldnorm_detail::log1mexp(x)<<"\n";
 int trials=0,differences=0;
 for(double start:{.1,1.,10.})for(double tol:{1e-4,1e-8,1e-12})for(bool absolute:{false,true}) {
  auto f=[](double x){return (x*x-2)/(2*x);};
  auto r=absolute?cppunivariate::newton_raphson_1d_abs(start,f,tol,100):cppunivariate::newton_raphson_1d(start,f,tol,100);
  double x=start;int i=1;
  for(;i<=100;++i){double step=f(x),next=x-step;bool stop=absolute?std::abs(step)<tol:std::abs((x-next)/x)<tol;x=next;if(stop)break;}
  ++trials;if(x!=r.x || i!=r.iterations)++differences;
 }
 std::cerr<<"newton_legacy_differences,"<<differences<<","<<trials<<"\n";
 auto rel=cppunivariate::newton_raphson_1d(0.,[](double){return 0.;},1e-12,10);
 auto abs=cppunivariate::newton_raphson_1d_abs(1.,[](double){return 0.;},1e-12,10);
 std::cerr<<"newton_zero_relative,"<<rel.converged<<","<<rel.iterations<<"\n";
 std::cerr<<"newton_zero_step_nonroot,"<<abs.converged<<","<<abs.iterations<<"\n";
 std::cerr<<"lambert_budget0,"<<cppunivariate::lambertw0(-.3,1e-12,0)<<"\n";
}
'''
(OUT/'probe.cpp').write_text(source)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
cmd=['g++','-std=c++17','-O3','-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,str(OUT/'probe.cpp'),'-o',str(OUT/'probe')]
subprocess.run(cmd,check=True)
# Exact binary64 inputs are promoted exactly to mpmath. Near branch point,
# conditioning must not be confused with errors in decimal input conversion.
zs=set([-1/math.e,math.nextafter(-1/math.e,0),-.3,-.1,-.001,-1e-10,-1e-100,-1e-300,-math.ulp(0.),0.,.1,1.,100.,1e300,-.4,1e-300])
for k in range(1,54):zs.add(-1/math.e*(1-2.**-k))
for k in range(0,325,3):zs.add(-10.**(-k))
for i in range(301):
 g=1+10**(-14+i*17/300);zs.add(-g*math.exp(-g))
cases=[(b,z) for b in (0,1) for z in sorted(zs)]
xs=[-1e6,-1000.,-100.,-31.,-30.,-10.,-1.,0.,1.,10.,100.,700.,710.]
cases += [(2,x) for x in xs]
proc=subprocess.run([str(OUT/'probe')],input=''.join(f'{b} {z:.17g}\n' for b,z in cases),text=True,capture_output=True,check=True)
(OUT/'raw.csv').write_text(proc.stdout);(OUT/'timing.txt').write_text(proc.stderr)
rows=[];summary={}
for line in proc.stdout.splitlines()[1:]:
 b,z,own,boost=map(float,line.split(','));b=int(b)
 if b==2:
  residual=float(abs(mp.digamma(mp.mpf(own))-mp.mpf(z))) if math.isfinite(own) and own>0 else None
  rows.append(dict(branch=b,z=z,own=own,boost=boost,reference=None,own_error=residual,boost_error=None));continue
 valid=mp.mpf(z)>=-1/mp.e and (b==0 or z<0)
 ref=mp.re(mp.lambertw(mp.mpf(z),-b)) if valid else None
 def error(v):return float(abs(mp.mpf(v)-ref)) if ref is not None and math.isfinite(v) else None
 rows.append(dict(branch=b,z=z,own=own,boost=boost,reference=float(ref) if ref is not None else None,own_error=error(own),boost_error=error(boost)))
for b in (0,1):
 rs=[r for r in rows if r['branch']==b and r['reference'] is not None]
 summary[str(b)]={k:sum(r[k+'_error'] is None for r in rs) for k in ('own','boost')}
 for k in ('own','boost'):
  good=[r for r in rs if r[k+'_error'] is not None]
  worst=max(good,key=lambda r:r[k+'_error'])
  summary[str(b)][k+'_worst']=worst
 summary[str(b)]['valid_points']=len(rs)
 summary[str(b)]['bit_equal_finite']=sum(r['own']==r['boost'] and math.isfinite(r['own']) for r in rs)
# Also quantify caller-coordinate error: gamma + W0(-gamma exp(-gamma)).
ziprows=[]
for r in rows:
 if r['branch']==0 and r['reference'] is not None and -1/math.e<=r['z']<0:
  # gamma inferred from the *rounded input*, so this measures W only.
  g=float(mp.re(mp.lambertw(mp.mpf(r['z']),-1)))*-1
  ziprows.append((g,r['own_error'],r['boost_error']))
summary['environment']={'revision':subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),'platform':platform.platform(),'python':sys.version,'mpmath':mp.__version__,'compiler':subprocess.check_output(['g++','--version'],text=True).splitlines()[0],'boost':proc.stdout.splitlines()[0], 'compile':cmd}
summary['timing_and_failures']=proc.stderr.splitlines()
summary['inverse_digamma']=[r for r in rows if r['branch']==2]
(OUT/'summary.json').write_text(json.dumps(summary,indent=2,allow_nan=True)+'\n')
with (OUT/'errors.csv').open('w') as f:
 w=csv.DictWriter(f,fieldnames=rows[0]);w.writeheader();w.writerows(rows)
print(json.dumps(summary,indent=2,allow_nan=True))
