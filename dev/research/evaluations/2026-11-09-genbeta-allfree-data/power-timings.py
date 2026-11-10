import json,subprocess,re,math
from pathlib import Path
root=Path.cwd();data=root/'dev/research/evaluations/2026-11-09-genbeta-allfree-data';scratch=root/'build/scratch';family=root/'univariateML-cpp/include/cppunivariate/distributions/genbeta'
s=(data/'mle-baseline.hpp.txt').read_text();s=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((family/m[1]).resolve())+'"',s);(scratch/'power-baseline.hpp').write_text(s)
body=r'''#include HEADER
#include <iostream>
#include <chrono>
int main(){std::vector<double>x;for(int i=0;i<300;++i){const double u=(i+.5)/300;x.push_back(.7*u*u);}
 const auto start=std::chrono::steady_clock::now();cppunivariate::GenbetaResult r{};
 for(int i=0;i<3;++i)r=cppunivariate::genbeta_mle({x.data(),x.size()},1);
 std::cout.precision(17);std::cout<<int(r.status)<<" "<<CERTIFIED<<" "<<r.logLik<<" "<<r.iterations<<" "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()/3<<" "<<(r.stop_detail.limit_coef.size()==2?r.stop_detail.limit_coef[0]:NAN)<<" "<<(r.stop_detail.limit_coef.size()==2?r.stop_detail.limit_coef[1]:NAN)<<"\n";}
'''
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip();out={}
for label,header in [('before',scratch/'power-baseline.hpp'),('after',family/'mle.hpp')]:
 src=scratch/('power-'+label+'.cpp');exe=scratch/('power-'+label)
 src.write_text(body.replace('HEADER','"'+str(header)+'"').replace('CERTIFIED','r.allfree_certified' if label=='after' else '0'))
 subprocess.run(['c++','-O2','-std=c++17','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(src),'-o',str(exe)],check=True)
 out[label]=list(map(float,subprocess.check_output([str(exe)],text=True).split()))
assert out['before'][0]==out['after'][0]==2
assert out['after'][1]==1
assert abs(out['before'][2]-out['after'][2])<1e-8
for i in (5,6):assert abs(out['before'][i]/out['after'][i]-1)<1e-10
out['columns']=['status','allfree_certified','loglik','work','milliseconds','power_endpoint','power_shape'];out['repetitions']=3;out['sample_size']=300
out['compiler']=subprocess.check_output(['c++','--version'],text=True).splitlines()[0]
(data/'power-timings.json').write_text(json.dumps(out,indent=2)+'\n');print(json.dumps(out,indent=2))
