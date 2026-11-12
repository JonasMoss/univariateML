"""Independent 80-digit enumeration of negative-exponent law and quantiles."""
from pathlib import Path
import json, math, subprocess
import mpmath as mp
mp.mp.dps = 80
root = Path.cwd()
dest = root / 'dev/research/evaluations/2026-11-12-modifiers-2-data'
source = root / 'build/scratch/zeta-quantile-reference.cpp'
source.write_text(r'''
#include "cppunivariate/distributions/zeta/truncation.hpp"
#include <iostream>
#include <iomanip>
int main() {
 std::cout<<std::setprecision(17);
 for(int n:{4,100,3000,10000})for(double s:{-2.,-7.,-std::nextafter(7.,INFINITY),-8.,-std::nextafter(8.,INFINITY),-100.}) {
  for(double p:{1e-100,.001,.1,.5,.9,.999})for(bool lower:{false,true}) {
   const double q=cppunivariate::zeta_window_eval('q',p,n,s,lower,false);
   const double lq=cppunivariate::zeta_window_eval('q',std::log(p),n,s,lower,true);
   std::cout<<n<<" "<<s<<" "<<p<<" "<<lower<<" "<<q<<" "<<lq<<"\n";
  }
 }
}
''')
boost = subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
exe = source.with_suffix('')
subprocess.run(['c++','-std=c++17','-O2','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(source),'-o',str(exe)],check=True)
cache = {}; results = []
for line in subprocess.check_output([str(exe)],text=True).splitlines():
    n,s,p,lower,q,lq = map(float,line.split());n=int(n);lower=bool(lower)
    key = n,s
    if key not in cache:
        weights = [mp.power(j,-mp.mpf(s)) for j in range(1,n+1)]
        total = mp.fsum(weights); cumulative = []; c=mp.mpf(0)
        for w in weights:
            c+=w; cumulative.append(c/total)
        cache[key] = cumulative
    cdf = cache[key];target=mp.mpf(p) if lower else 1-mp.mpf(p)
    # p=1e-100 in the upper tail requires more than 80 digits to form 1-p.
    if not lower and p<1e-80: expected=n
    else: expected=next((i+1 for i,v in enumerate(cdf) if v>=target),n)
    assert q==expected and lq==expected,(n,s,p,lower,q,lq,expected)
    results.append(dict(N=n,s=s,p=p,lower_tail=lower,quantile=int(q),reference=expected))
(dest/'zeta-quantile-reference.json').write_text(json.dumps(dict(precision=80,comparisons=results),indent=2)+'\n')
print(len(results),'negative-exponent quantile comparisons passed')
