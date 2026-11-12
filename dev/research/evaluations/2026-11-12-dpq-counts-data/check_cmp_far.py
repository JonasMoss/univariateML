"""Standalone native/double probes and independent 80-digit far-weight check.
Run through /tmp/umvl-cpu/run after the integration commands have ended.
"""
import json,subprocess,math,sys,hashlib
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
work=ROOT/'build/scratch/cmp-far';work.mkdir(exist_ok=True)
source=work/'probe.cpp'
source.write_text('''#include <iostream>
#include <iomanip>
#include "cppunivariate/distributions/cmp/dpqr.hpp"
int main(){const double x=std::numeric_limits<double>::max();std::cout<<std::setprecision(17)
<<cppunivariate::distributions::dcmp(x,1,1e-8,true)<<"\\n"
<<cppunivariate::distributions::pcmp(x,1,1e-8,false,true)<<"\\n";}
''')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
table=json.loads((ROOT/'univariateML-cpp/include/cppunivariate/distributions/cmp/truth.json').read_text())
index=table['parameters'].index([1.,1e-8]);log_p0=next(r[2] for r in table['values']if r[0]==index and r[1]==0)
mp.mp.dps=80
x=mp.mpf(sys.float_info.max);nu=mp.mpf(float(1e-8))
reference=-nu*mp.loggamma(x+1)+mp.mpf(float(log_p0))
width=-mp.log1p(-mp.exp(-nu*mp.log(x+2)))
rows=[]
for mode in ['native','double']:
 exe=work/mode
 flags=['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if mode=='double' else []
 subprocess.run(['ccache','c++','-std=c++17','-O2',*flags,'-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,str(source),'-o',str(exe)],check=True)
 actual=list(map(float,subprocess.check_output([str(exe)],text=True).splitlines()))
 for op,value in zip(['d','p-upper'],actual):
  assert math.isfinite(value)
  error=(abs(mp.mpf(value)-reference)+(width+nu*mp.log(x+2) if op=='p-upper' else 0))/abs(reference)
  assert error<mp.mpf('1e-10'),(mode,op,error)
  rows.append(dict(mode=mode,op=op,actual=value,normalized_error=mp.nstr(error,30),binary_sha256=hashlib.sha256(exe.read_bytes()).hexdigest()))
js=HERE/'cmp-far-js.json'
if js.exists():
 values=json.loads(js.read_text())
 for op,value in zip(['d','p-upper'],values['actual']):
  assert math.isfinite(value)
  error=(abs(mp.mpf(value)-reference)+(width+nu*mp.log(x+2) if op=='p-upper' else 0))/abs(reference)
  assert error<mp.mpf('1e-10')
  rows.append(dict(mode='JS',op=op,actual=value,normalized_error=mp.nstr(error,30)))
result=dict(reference_log_mass=mp.nstr(reference,75),upper_tail_log_width=mp.nstr(width,40),records=rows)
(HERE/'cmp-far-reference.json').write_text(json.dumps(result,indent=2,allow_nan=False)+'\n')
print(json.dumps(result,indent=2))
