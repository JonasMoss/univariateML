"""Independent 1000-digit checks of natural tails after extreme rescaling.
Run through /tmp/umvl-cpu/run from the repository root. No dependencies are
installed; the generated probe and executable stay under build/scratch.
"""
import argparse, hashlib, json, math, subprocess
from pathlib import Path
import mpmath as mp

parser=argparse.ArgumentParser()
parser.add_argument('--output',type=Path,default=Path(__file__).with_suffix('.json'))
args=parser.parse_args()
mp.mp.dps=1000
root=Path.cwd()
scratch=root/'build/scratch'
scratch.mkdir(parents=True,exist_ok=True)
source=scratch/'pgw-range-probe.cpp'
source.write_text('''#include "cppunivariate/distributions/pgw/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main() {
  double x,a,b,c;
  std::cout<<std::setprecision(17);
  while(std::cin>>x>>a>>b>>c)
    std::cout<<cppunivariate::distributions::ppgw(x,a,b,c,false,false)<<" "
      <<cppunivariate::distributions::ppgw(x,a,b,c,false,true)<<" "
      <<cppunivariate::distributions::dpgw(x,a,b,c,true)<<"\\n";
}
''')
subprocess.run(['c++','-std=c++17','-O2','-I'+str(root/'univariateML-cpp/include'),str(source),'-o',str(scratch/'pgw-range-probe')],check=True,capture_output=True)
cases=[]
def add(x,a,b,label):
    xx,aa,bb=map(mp.mpf,(x,a,b))
    c=float(mp.log(700)/mp.log1p((xx/aa)**bb))
    if 0<c<math.inf:
        cases.append((x,a,b,c,label))
for a in (1e-300,1e300):
    for b in (.1,1.,10.,100.): add(3*a,a,b,'far ratio, rescaled')
for a in (1e-300,1e300):
    cases.append((3*a,a,math.log(700)/math.log(3),1.,'Weibull reduction after rescaling'))
for b in (1.,2.,10.,100.,1e5,1e12):
    x=math.exp((math.log(math.log(700))-math.log(1e308))/b)
    add(x,1.,b,'tiny power, huge exponent')
for a in (1.,1e-300,1e200,1e300):
    for b in (1e18,3e18): add(math.nextafter(a,0.),a,b,'near scale, huge shape')
inputs=''.join(' '.join(repr(v) for v in row[:4])+'\n' for row in cases)
run=subprocess.run([str(scratch/'pgw-range-probe')],input=inputs,text=True,capture_output=True,check=True)
rows=[]
for case,line in zip(cases,run.stdout.splitlines(),strict=True):
    x,a,b,c,label=case
    xx,aa,bb,cc=map(mp.mpf,(x,a,b,c))
    z=bb*mp.log(xx/aa)
    L=mp.log1p(mp.exp(z));v=cc*L;h=mp.expm1(v)
    logd=mp.log(bb*cc/xx)+z-L+v-h
    truth=float(mp.exp(-h))
    upper,logupper,ld=map(float,line.split())
    error=abs(upper-truth)/truth
    rows.append(dict(label=label,x=x,scale=a,shape=b,exponent=c,upper=upper,reference_upper=truth,relative_upper_error=error,log_upper_error=abs(logupper+float(h)),log_density_error=abs(ld-float(logd))))
header=root/'univariateML-cpp/include/cppunivariate/distributions/pgw/dpqr.hpp'
result=dict(dps=mp.mp.dps,header_sha256=hashlib.sha256(header.read_bytes()).hexdigest(),cases=rows,max_relative_upper_error=max(r['relative_upper_error'] for r in rows),failed=sum(r['relative_upper_error']>1e-10 for r in rows))
args.output.write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps({k:result[k] for k in ('max_relative_upper_error','failed')}))

if result['failed']: raise SystemExit(1)
