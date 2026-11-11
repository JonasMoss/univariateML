"""Generate an instrumented SSTD precision check; --before uses the saved old sum.

Compile the output with -I. -IunivariateML-cpp/include and Boost headers.
The unchanged proposal search is followed by the actual precision gate.
"""
import json,sys
from pathlib import Path
D=Path(__file__).resolve().parent;root=D.parents[3]
h=(root/'univariateML-cpp/include/cppunivariate/distributions/sstd/local_accuracy.hpp').read_text()
p=h[h.index('template<class Objective,class Error>'):h.index('inline double log_error')].replace('inline bool polish','inline bool trace_polish')
p=p.replace('return false;','return trace_failure(__LINE__);')
p=p.replace('const double e=error(p,s);','const double e=error(p,s); if(it<12)std::cerr<<"it "<<it<<" error "<<e<<"\\n";')
p=p.replace('if(2*noise+error(q,remaining)>tol*.75)','std::cerr<<"noise "<<noise<<" remaining "<<error(q,remaining)<<"\\n"; if(2*noise+error(q,remaining)>tol*.75)')
print('#include <iostream>\n#include <iomanip>\n#include <string>\n#include "cppunivariate/distributions/sstd/mle.hpp"')
print('#include "dev/research/evaluations/2026-11-10-acc-allfree-data/sstd-skew-objective-before.hpp"')
print('namespace cppunivariate { namespace local_accuracy { inline bool trace_failure(int line){std::cerr<<"FAIL "<<line<<"\\n";return false;}')
print(p);print('}}\nint main(){std::cerr<<std::setprecision(17);std::cout<<std::setprecision(17);')
for c in map(json.loads,(D/'sstd-full-tier-cases.jsonl').read_text().splitlines()):
 if c['seed'] not in (5007,'replication'):continue
 repeat=c['seed']=='replication';name=str(c['seed'])
 print('{std::vector<double>x{'+','.join(map(repr,c['data']))+'};')
 if repeat:print('auto base=x;for(int k=1;k<100;++k)x.insert(x.end(),base.begin(),base.end());')
 print('std::cerr<<"CASE '+name+'\\n";')
 print('auto r=cppunivariate::sstd_mle_impl({x.data(),x.size()},1e-10,200,false);std::cerr<<"proposal "<<int(r.status)<<" "<<r.mu<<" "<<r.sigma<<" "<<r.nu<<" "<<r.xi<<"\\n";')
 print('auto m=cppunivariate::norm_detail::moments({x.data(),x.size()});std::vector<double>z;for(double v:x)z.push_back((v-m.mean)/m.sd);std::array<double,4>p{{(r.mu-m.mean)/m.sd,std::log(r.sigma/m.sd),std::log(r.nu-2),std::log(r.xi)}};')
 obj='uncompensated_skew_objective' if '--before' in sys.argv else 'skew_objective'
 mapped='' if '--before' in sys.argv else 'sf[2]*=std::exp(v[2])/(2+std::exp(v[2]));'
 print('auto objective=[&](const std::array<cppunivariate::local_accuracy::Jet,4>&q){return cppunivariate::local_accuracy::'+obj+'(z,q,true);};auto error=[&](const std::array<double,4>&v,const std::array<double,4>&s){auto q=v;q[0]+=m.mean/m.sd;auto sf=s;'+mapped+'return cppunivariate::local_accuracy::location_error(q,sf);};')
 print('bool ok=cppunivariate::local_accuracy::trace_polish(p,4,1e-10,200,objective,error);')
 print('std::array<double,4> estimate{{m.mean+m.sd*p[0],m.sd*std::exp(p[1]),2+std::exp(p[2]),std::exp(p[3])}};')
 print('std::cout<<"{\\"id\\":\\"'+name+'\\",\\"n\\":"<<x.size()<<",\\"error\\":\\""<<(ok?"":"accuracy failure")<<"\\",\\"estimates\\":{";const std::array<std::string,4> names{{"mean","sd","nu","xi"}};for(int j=0;j<4;++j){if(j)std::cout<<",";std::cout<<"\\\""<<names[j]<<"\\\":";if(std::isfinite(estimate[j]))std::cout<<estimate[j];else std::cout<<"null";}std::cout<<"}}\\n";}')
print('}')
