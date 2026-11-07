"""Reproduce narrow-width certificate stops without modifying library headers.
The temporary instrumented header records only the failing gate in stop fields.
"""
import json,pathlib,re,subprocess,os
ROOT=pathlib.Path(__file__).resolve().parents[4];HERE=pathlib.Path(__file__).resolve().parent
BUILD=ROOT/'build/scratch';BUILD.mkdir(parents=True,exist_ok=True)
p=ROOT/'univariateML-cpp/include/cppunivariate/distributions/trgamma/mle.hpp'
s=p.read_text()
s=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((p.parent/m[1]).resolve())+'"',s)
start=s.index('inline TrGammaResult fit_free');end=s.index('}  // namespace trgamma_detail',start)
part=s[start:end].replace('int evaluations = 0;','int evaluations = 0; int debug_code=0; double dw=NAN,dk=NAN,ds=NAN;')
count=[0]
def tag(match):
    count[0]+=1
    return f'failure=(debug_code={count[0]},true);'
part=re.sub(r'\bfailure\s*=\s*true;',tag,part)
pattern=r'if\(!enclosed \|\|.*?coefficient_failure=true;'
replacement='''dw=(right-left)/best.t;dk=shape_hi/shape_lo-1;ds=high-low;
      if(!enclosed || std::abs(log_minus_digamma(best.shape)-best.K)>1e-12*best.K || dw>1e-14 || dk>2e-11 || (represented_scale && ds>1e-10)) {
        coefficient_failure=true;
        debug_code=!enclosed?101:(std::abs(log_minus_digamma(best.shape)-best.K)>1e-12*best.K?106:(dw>1e-14?103:(dk>2e-11?104:105)));
      }'''
part,n=re.subn(pattern,replacement,part,flags=re.S);assert n==1
part=part.replace('    return out;','    out.stop_detail.segment_loglik_upper=debug_code;out.stop_detail.limit_coef={dw,dk,ds};return out;',1)
s=s[:start]+part+s[end:]
(BUILD/'trgamma-debug.hpp').write_text(s)
dp=p.with_name('dpqr.hpp')
def dp_include(m):
    target=BUILD/'trgamma-debug.hpp' if m[1]=='mle.hpp' else (dp.parent/m[1]).resolve()
    return '#include "'+str(target)+'"'
(BUILD/'trgamma-debug-dpqr.hpp').write_text(re.sub(r'#include "([^"]+)"',dp_include,dp.read_text()))

source=r'''
#include "trgamma-debug.hpp"
#include "trgamma-debug-dpqr.hpp"
#include <random>
#include <iostream>
#include <iomanip>
int main(){std::mt19937_64 engine(20261029);const double params[][3]={{2,1.5,2},{.3,4,1},{8,.4,3},{1,1,1},{50,.2,.001}};
for(const auto& p:params)for(int n:{8,30,150})for(int rep=0;rep<4;++rep){std::vector<double>x;
for(int i=0;i<n;++i){double u=(static_cast<double>(engine()>>11)+.5)/9007199254740992.;x.push_back(cppunivariate::distributions::qtrgamma(u,p[0],p[1],p[2],true,false));}
auto r=cppunivariate::trgamma_mle({x.data(),x.size()});
if(r.status==cppunivariate::TrGammaStatus::IterationFailure){std::cout<<std::setprecision(17)<<p[0]<<" "<<n<<" "<<rep<<" "<<r.stop_detail.budget_exhausted<<" "<<r.stop_detail.segment_loglik_upper;
for(double v:r.stop_detail.limit_coef)std::cout<<" "<<v;std::cout<<"\n";}}
}
'''
(BUILD/'diagnose.cpp').write_text(source)
bh=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
subprocess.run(['c++','-std=c++17','-O2','-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS','-I'+str(BUILD),'-I'+str(ROOT/'univariateML-cpp/include'),'-I'+bh,str(BUILD/'diagnose.cpp'),'-o',str(BUILD/'diagnose-stops')],check=True)
result=subprocess.check_output([str(BUILD/'diagnose-stops')],text=True)
print(result)
(HERE/'numeric-stops.json').write_text(json.dumps(dict(seed=20261029,macro='BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS',columns=['shape1_sim','n','rep','budget','gate','power_relative_width','shape_relative_width','logscale_width'],rows=[[float(x) for x in line.split()] for line in result.splitlines()]),indent=2)+'\n')

# Cross-width coefficient accuracy is independent of the C++ regression suite.
subprocess.run(['python3',str(HERE/'check.py')],env={**os.environ,'UMVL_CHECK_NO_LD':'1'},check=True)
