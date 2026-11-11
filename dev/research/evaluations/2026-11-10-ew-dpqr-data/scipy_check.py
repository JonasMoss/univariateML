"""Independent SciPy spot-check of the C++ scalar entry points (not truth)."""
from pathlib import Path
import subprocess, json
import scipy
from scipy.stats import exponweib
root=Path(__file__).resolve().parents[4]
source=root/'build/scratch/ew-spot.cpp'
binary=root/'build/scratch/ew-spot'
source.write_text('''#include "cppunivariate/distributions/expweibull/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main(){ double a,b,c,x,p; while(std::cin>>a>>b>>c>>x>>p) {
std::cout<<std::setprecision(17)<<cppunivariate::distributions::dexpweibull(x,a,b,c,true)<<" "
<<cppunivariate::distributions::pexpweibull(x,a,b,c,true,true)<<" "
<<cppunivariate::distributions::pexpweibull(x,a,b,c,false,true)<<" "
<<cppunivariate::distributions::qexpweibull(p,a,b,c,true,false)<<"\\n"; }}''')
subprocess.run(['c++','-std=c++17','-O2','-I'+str(root/'univariateML-cpp/include'),str(source),'-o',str(binary)],check=True)
cases=[(2,1.5,2.5,.1,.01),(2,1.5,2.5,2,.5),(2,1.5,2.5,10,.99),(1,.2,5,.01,.001),(3,10,.1,3,.9),(1,100,100,1,.8)]
output=subprocess.check_output([str(binary)],input=''.join(' '.join(map(str,r))+'\n' for r in cases),text=True)
rows=[]
for case,line in zip(cases,output.splitlines()):
    a,b,c,x,p=case
    actual=list(map(float,line.split()))
    ref=[exponweib.logpdf(x,c,b,scale=a),exponweib.logcdf(x,c,b,scale=a),exponweib.logsf(x,c,b,scale=a),exponweib.ppf(p,c,b,scale=a)]
    errors=[abs(u-v)/max(1,abs(v)) for u,v in zip(actual,ref)]
    assert max(errors)<2e-12,(case,actual,ref)
    rows.append(dict(parameters=dict(scale=a,shape=b,exponent=c),x=x,p=p,cpp=actual,scipy=ref,max_scaled_error=max(errors)))
result=dict(scipy_version=scipy.__version__,columns=['log_density','log_cdf','log_survival','quantile'],rows=rows)
Path(__file__).with_name('scipy-check.json').write_text(json.dumps(result,indent=2)+'\n')
print('SciPy:',len(rows),'cases; max scaled error',max(r['max_scaled_error'] for r in rows))
