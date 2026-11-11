#!/usr/bin/env python3
"""Generate the frozen audit coefficient cases for this job (no refitting oracle)."""
import gzip,json,re
from pathlib import Path
root=Path(__file__).resolve().parents[4]; here=Path(__file__).resolve().parent
ids='lomax kumar weibull gumbel fatigue cmb pig foldnorm'.split()
old=here.parent/'2026-11-09-fit-audit-rerun-data'
cases=json.load(gzip.open(here.parent/'2026-11-07-fit-audit-data/cases.json.gz','rt'))
refs=[r for r in map(json.loads,gzip.open(old/'reference.jsonl.gz','rt')) if r['family'] in ids and r.get('parameter_relative_error',r.get('unverified_stationary_displacement',0))>1e-10 and ('refined_theta' in r or 'better_theta' in r)]
selected=[]
for r in refs:
 c=cases[r['index']];assert c['family']==r['family'];selected.append(dict(case=c,reference=r))
(here/'replay-cases.json').write_text(json.dumps(selected,indent=2)+'\n')
print('#include <iostream>\n#include <iomanip>')
for id in ids: print(f'#include "bindings/common/distributions/{id}/fit.hpp"')
print('int main(){std::cout<<std::setprecision(17);')
for row in selected:
 c=row['case'];id=c['family'];text=(root/f'bindings/common/distributions/{id}/fit.hpp').read_text();sig=re.search(r'inline FitResult fit_'+id+r'\((.*?)\) \{',text,re.S)[1];integer='cppunivariate::ispan' in sig
 print('{std::vector<'+('int' if integer else 'double')+'> x{'+','.join(repr(v) for v in c['data'])+'};')
 call=['{x.data(),x.size()}']
 for arg in sig.split(',')[1:]:
  decl,default=arg.strip().split('=',1);typ,name=decl.strip().split();key=name.removesuffix('_arg')
  if typ=='Held':call.append('{true,'+repr(c['held'][key])+',1}' if key in c['held'] else '{}')
  # omit default controls so the before binary uses its own defaults
  else:break
 print('auto r=umvl_binding::fit_'+id+'('+','.join(call)+');')
 print('std::cout<<"{\\"index\\":'+str(c['index'])+',\\"id\\":\\"'+id+'\\",\\"estimates\\":{";bool first=true;for(auto v:r.estimates){if(!first)std::cout<<",";first=false;std::cout<<"\\\""<<v.first<<"\\\":";if(std::isfinite(v.second))std::cout<<v.second;else std::cout<<"null";}std::cout<<"},\\"error\\":\\""<<r.error<<"\\"}\\n";}')
print('}')
