#!/usr/bin/env python3
"""Reproduce no-long-double Pareto IV coefficient drift on HEAD and worktree.
Run through /tmp/umvl-cpu/run; all build products stay under build/scratch.
"""
from pathlib import Path
import io,json,subprocess,tarfile
ROOT=Path(__file__).resolve().parents[2]
OUT=ROOT/'build/scratch/step7-pareto4';OUT.mkdir(parents=True,exist_ok=True)
base=OUT/'baseline';base.mkdir(exist_ok=True)
with tarfile.open(fileobj=io.BytesIO(subprocess.check_output(['git','archive','HEAD','univariateML-cpp/include'],cwd=ROOT))) as archive:
 archive.extractall(base,filter='data')
source=r'''
#include "cppunivariate/distributions/pareto4/mle.hpp"
#include "json_loader.hpp"
#include <iostream>
#include <iomanip>
int main() {
 const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/pareto4/golden.json");
 const auto r=cppunivariate::pareto4_mle(cppunivariate::span{g.data.data(),g.data.size()},g.estimates.at("min"),1e-10,1000);
 std::cout<<std::setprecision(17)<<r.shape1<<","<<r.shape2<<","<<r.scale<<","<<r.logLik<<","<<int(r.status)<<"\n";
}
'''
(OUT/'probe.cpp').write_text(source)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
values=[]
for name,inc in [('before',base/'univariateML-cpp/include'),('after',ROOT/'univariateML-cpp/include')]:
 subprocess.run(['g++','-std=c++17','-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS','-I'+str(inc),'-I'+str(ROOT/'univariateML-cpp/tests'),'-I'+boost,str(OUT/'probe.cpp'),'-o',str(OUT/name)],check=True)
 output=subprocess.check_output([str(OUT/name)],cwd=ROOT,text=True)
 (OUT/(name+'.csv')).write_text(output);values.append(output)
assert values[0]==values[1], 'phase A changed no-long-double Pareto IV output'
actual=list(map(float,values[1].strip().split(',')))
golden=json.loads((ROOT/'univariateML-cpp/include/cppunivariate/distributions/pareto4/golden.json').read_text())
expected=[golden['estimates'][key] for key in ['shape1','shape2','scale']]+[golden['logLik']]
summary={'revision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(), 'byte_identical_to_baseline':True,
 'values':dict(zip(['shape1','shape2','scale','logLik','status'],actual)),
 'golden':dict(zip(['shape1','shape2','scale','logLik'],expected)),
 'relative_errors':dict(zip(['shape1','shape2','scale','logLik'],[abs(a-b)/abs(b) for a,b in zip(actual,expected)]))}
# Independent analytic natural-coordinate total score, matching the
# mathematical formula in bindings/js/validation/check-wasm-scores.py.
import mpmath as mp
mp.mp.dps=60
def likelihood_score(params):
 k,c,scale=(mp.mpf(params[key]) for key in ['shape1','shape2','scale'])
 threshold=mp.mpf(golden['estimates']['min'])
 ll=mp.mpf(0); score=[mp.mpf(0)]*3
 for value in golden['data']:
  x=mp.mpf(value)-threshold; lx=mp.log(x/scale)
  y=mp.exp(c*lx); weight=y/(1+y)
  ll+=mp.log(k)+mp.log(c)-mp.log(scale)+(c-1)*lx-(k+1)*mp.log1p(y)
  score[0]+=1/k-mp.log1p(y)
  score[1]+=1/c+lx-(k+1)*weight*lx
  score[2]+=(-c+(k+1)*c*weight)/scale
 return ll,mp.sqrt(sum(v*v for v in score)),score
ll,score,components=likelihood_score(summary['values']);old_ll,old_score,_=likelihood_score(golden['estimates'])
profile_norm=mp.sqrt((mp.mpf(actual[1])*components[1])**2+(mp.mpf(actual[2])*components[2])**2)
summary['independent_check']={'dps':60,'coordinates':['shape1','shape2','scale'],
 'normalization':'total log likelihood and free total score in declared coefficients',
 'actual_score_norm':mp.nstr(score,20),'golden_score_norm':mp.nstr(old_score,20),
 'total_log_profile_score_norm':mp.nstr(profile_norm,20),'profile_coordinates':['log(shape2)','log(scale/geometric_mean)'],
 'profile_score_caveat':'envelope components; shape1 is profiled, with its numerical score recorded below',
 'natural_score_components':[mp.nstr(v,20) for v in components],
 'logLik_difference':mp.nstr(ll-old_ll,20),'reported_logLik_error':mp.nstr(mp.mpf(actual[3])-ll,20)}
(OUT/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
