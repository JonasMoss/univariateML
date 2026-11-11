"""Generate replay source from frozen 1e-10 audit coefficient cases.

Compile with -I. -IunivariateML-cpp/include (and Boost include if needed).
Baseline uses an isolated header snapshot of commit c60d92ea;
never modifies the checkout. Output JSON lines are compared to audit references.
"""
from pathlib import Path
import json,gzip
root=Path(__file__).resolve().parents[4]
out=Path(__file__).resolve().parent
cases=json.loads((out/'audit-cases.json').read_text())
ids=['slash','sstd','sged','st','bnbinom','gb2']
print('#include <iostream>\n#include <iomanip>')
for id in ids:print(f'#include "bindings/common/distributions/{id}/fit.hpp"')
print('int main(){std::cout<<std::setprecision(17);')
for row in cases:
 c=row['case'];id=c['family'];integer=id=='bnbinom';names=list(json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{id}/golden.json').read_text())['estimates'])
 vals=','.join(str(v) for v in c['data']);held=','.join('{true,'+str(c['held'][k])+',1}' if k in c['held'] else '{}' for k in names)
 print('{std::vector<'+('int' if integer else 'double')+'> x{'+vals+'};')
 print(f'auto r=umvl_binding::fit_{id}({{x.data(),x.size()}},{held});')
 print('std::cout<<"{\\"index\\":'+str(c['index'])+',\\"family\\":\\"'+id+'\\",\\"error\\":\\""<<r.error<<"\\",\\"estimates\\":{";bool first=true;for(auto v:r.estimates){if(!first)std::cout<<",";first=false;std::cout<<"\\\""<<v.first<<"\\\":";if(std::isfinite(v.second))std::cout<<v.second;else std::cout<<"null";}std::cout<<"}}"<<std::endl;}')
print('}')
