"""Generate public-log-coordinate score roundoff probes for held GB2 cases."""
import json
from pathlib import Path
D=Path(__file__).resolve().parent
a={r['case']['index']:r for r in json.loads((D/'audit-cases.json').read_text())}
print('#include <iostream>\n#include <iomanip>\n#include "cppunivariate/distributions/gb2/mle.hpp"\nint main(){std::cout<<std::setprecision(17);')
for idx in [9518,2215]:
 e=a[idx];c=e['case'];v=list(map(float,e['reference']['refined_theta']));free=[j for j,k in enumerate(['shape1','shape2','shape3','scale']) if k not in c['held']]
 print('{std::vector<double>x{'+','.join(map(str,c['data']))+'};std::array<double,4>p{};')
 for j,k in enumerate(free):print(f'p[{j}]=std::log({v[k]});')
 print('auto z=cppunivariate::local_accuracy::variables(p);std::array<cppunivariate::local_accuracy::Jet,4> v;')
 for k in range(4):print(f'v[{k}]='+('cppunivariate::local_accuracy::exp(z['+str(free.index(k))+'])' if k in free else str(v[k]))+';')
 print('auto f=x.size()*(cppunivariate::local_accuracy::logbeta(v[2],v[0])-cppunivariate::local_accuracy::log(v[1]));for(double y:x){auto t=v[1]*(std::log(y)-cppunivariate::local_accuracy::log(v[3]));f=f+v[2]*cppunivariate::local_accuracy::softplus(-t)+v[0]*cppunivariate::local_accuracy::softplus(t);}')
 print('std::array<double,4>s{};bool pd=cppunivariate::local_accuracy::step(f,3,s);')
 print('std::cout<<"{\\"index\\":'+str(idx)+',\\"point\\":[";for(int j=0;j<3;++j){if(j)std::cout<<",";std::cout<<p[j];}std::cout<<"],\\"gradient\\":[";for(int j=0;j<3;++j){if(j)std::cout<<",";std::cout<<f.g[j];}std::cout<<"],\\"step\\":[";for(int j=0;j<3;++j){if(j)std::cout<<",";std::cout<<s[j];}std::cout<<"],\\"pd\\":"<<pd<<"}"<<std::endl;}')
print('}')
