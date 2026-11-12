"""Focused replay of the original failed exact-symmetry regression; one slot."""
import json,subprocess
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
WORK=ROOT/'build/scratch/bnb-loglik'
g=json.loads((ROOT/'univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json').read_text())
source='''#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::vector<int> x{DATA};for(double h:{1.,5.,1.0672234787479615,5.3647338351087397}){
 auto a=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,h,NAN,NAN);
 auto b=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,NAN,NAN,h);
 bool ok=a.status==cppunivariate::BnbinomStatus::Ok && b.status==cppunivariate::BnbinomStatus::Ok && a.size==h && b.beta==h && a.alpha==b.alpha && a.beta==b.size && a.logLik==b.logLik;
 ok=ok && a.stop_detail.loglik_lower<=a.logLik && a.stop_detail.loglik_upper>=a.logLik;
 std::cout<<std::setprecision(17)<<h<<" "<<a.logLik<<" "<<b.logLik<<" "<<ok<<"\\n";
 if(!ok)return 1;
}}
'''.replace('DATA',','.join(map(str,g['data'])))
(WORK/'symmetry.cpp').write_text(source)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
records={}
for mode in ['native','double']:
 exe=WORK/f'symmetry-{mode}'
 subprocess.run(['g++','-std=c++17','-O2','-DNDEBUG','-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,*(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if mode=='double' else []),str(WORK/'symmetry.cpp'),'-o',str(exe)],check=True)
 output=subprocess.check_output([str(exe)],text=True)
 records[mode]=[dict(held=float(h),size_held_logLik=float(a),beta_held_logLik=float(b),passed=bool(int(ok))) for h,a,b,ok in map(str.split,output.splitlines())]
 print(mode,'4 exact symmetry checks passed',flush=True)
(HERE/'symmetry.json').write_text(json.dumps(records,indent=2)+'\n')
