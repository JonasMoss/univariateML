#!/usr/bin/env python3
"""Generate a standalone default-binding fit capture (no R, no fitted oracle).

python3 .../capture.py > build/scratch/capture.cpp
Compile with -I. -IunivariateML-cpp/include and Boost headers.
Prints one JSON object per family; the caller retains native/double captures.
"""
import json,re
from pathlib import Path
root=Path(__file__).resolve().parents[4]
dists=root/'univariateML-cpp/include/cppunivariate/distributions'
print('#include <iostream>\n#include <iomanip>\n#include <limits>')
ids=sorted(p.parent.name for p in dists.glob('*/golden.json'))
for id in ids: print(f'#include "bindings/common/distributions/{id}/fit.hpp"')
print('int main(){std::cout<<std::setprecision(17);')
for id in ids:
 g=json.loads((dists/id/'golden.json').read_text());args=g.get('fit_args',{})
 text=(root/f'bindings/common/distributions/{id}/fit.hpp').read_text()
 sig=re.search(r'inline FitResult fit_'+id+r'\((.*?)\) \{',text,re.S)[1]
 integer='cppunivariate::ispan x' in sig
 def literal(v):
  if v in ('Inf','Infinity'):return 'INFINITY'
  if v in ('-Inf','-Infinity'):return '-INFINITY'
  return repr(v)
 print('{std::vector<'+('int' if integer else 'double')+'> x{'+','.join(literal(v) for v in g['data'])+'};')
 call=['{x.data(),x.size()}']
 for arg in sig.split(',')[1:]:
  decl,default=arg.strip().split('=',1);typ,name=decl.strip().split();key=name.removesuffix('_arg')
  if typ=='Held': call.append('{true,'+literal(args[key])+',1}' if args.get(key) is not None else '{}')
  else: call.append(literal(args[key]) if args.get(key) is not None else default.strip())
 print(f'auto r=umvl_binding::fit_{id}('+','.join(call)+');')
 print('std::cout<<"{\\"id\\":\\"'+id+'\\",\\"estimates\\":{";bool first=true;for(auto v:r.estimates){if(!first)std::cout<<",";first=false;std::cout<<"\\\""<<v.first<<"\\\":";if(std::isfinite(v.second))std::cout<<v.second;else if(std::isinf(v.second))std::cout<<"\\\""<<v.second<<"\\\"";else std::cout<<"null";}std::cout<<"},\\"logLik\\":";if(std::isfinite(r.logLik))std::cout<<r.logLik;else std::cout<<"null";std::cout<<",\\"error\\":\\""<<r.error<<"\\"}\\n";}')
print('}')
