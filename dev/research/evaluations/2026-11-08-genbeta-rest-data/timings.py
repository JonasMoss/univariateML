import json,re,subprocess,math
from pathlib import Path
root=Path.cwd();data=root/'dev/research/evaluations/2026-11-08-genbeta-rest-data';scratch=root/'build/scratch'
f=root/'univariateML-cpp/include/cppunivariate/distributions/genbeta'
s=(data/'mle-baseline.hpp.txt').read_text();s=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((f/m[1]).resolve())+'"',s);(scratch/'baseline.hpp').write_text(s)
g=json.loads((f/'golden.json').read_text())
body='''#include HEADER
#include <iostream>
#include <chrono>
int main(){std::vector<double>x{DATA};std::cout.precision(17);for(double q:{NAN,.01,.1,.3,.7,2.}){auto start=std::chrono::steady_clock::now();cppunivariate::GenbetaResult r{};for(int i=0;i<10;++i)r=cppunivariate::genbeta_mle({x.data(),x.size()},1,1e-7,300,NAN,q);std::cout<<q<<" "<<int(r.status)<<" "<<int(r.certificate_failure)<<" "<<r.shape1<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.logLik<<" "<<r.iterations<<" "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()/10<<"\\n";}}
'''.replace('DATA',','.join(map(repr,g['data']))).replace('{NAN,','{double(NAN),')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
result={}
for label,header in [('before',scratch/'baseline.hpp'),('after',f/'mle.hpp')]:
 src=scratch/(label+'.cpp');exe=scratch/label;src.write_text(body.replace('HEADER','"'+str(header)+'"'))
 subprocess.run(['c++','-O2','-std=c++17','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(src),'-o',str(exe)],check=True)
 result[label]=[list(map(float,line.split())) for line in subprocess.check_output([str(exe)],text=True).splitlines()]
assert result['before'][0][1:8]==result['after'][0][1:8], 'default drift'
for row in (result['before'][0],result['after'][0]):
 for i,key in [(3,'shape1'),(4,'shape2'),(5,'shape3')]:
  assert abs(row[i]/g['estimates'][key]-1)<1e-5
 assert abs(row[6]-g['logLik'])<1e-8
for label in ('before','after'):
 result[label]=[[v if math.isfinite(v) else None for v in row] for row in result[label]]
result['repetitions']=10
result['sample_size']=len(g['data'])
result['columns']=['held_q_nan_default','status','failure','p','q','power','logLik','work','milliseconds'];result['compiler']=subprocess.check_output(['c++','--version'],text=True).splitlines()[0]
(data/'timings.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result,indent=2))
