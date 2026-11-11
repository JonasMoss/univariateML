#!/usr/bin/env python3
"""Emit frozen 1e-10 coefficient failures as calls through the current binding seam.

python replay.py > build/scratch/replay.cpp
Compile once against the archived HEAD headers and once against current headers.
"""
import gzip,json,re
from pathlib import Path
root=Path(__file__).resolve().parents[4]
old=root/'dev/research/evaluations/2026-11-09-fit-audit-rerun-data'
ids='sn skellam sgomp dgamma pareto3 zinb zmnbinom ztnbinom ht emg genbeta'.split()
refs=[x for x in map(json.loads,gzip.open(old/'reference.jsonl.gz','rt')) if x['family'] in ids and x.get('coefficient_error_verified') and x.get('parameter_relative_error',0)>1e-10]
cases={x['index']:x for x in json.load(gzip.open(root/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz','rt'))}
(root/'dev/research/evaluations/2026-11-10-acc-profile-data/replay-references.json').write_text(json.dumps([dict(reference=r,case=cases[r['index']]) for r in refs],indent=2)+'\n')
print('#include <iostream>\n#include <iomanip>')
for id in ids:print(f'#include "bindings/common/distributions/{id}/fit.hpp"')
print('int main(){std::cout<<std::setprecision(17);')
for r in refs:
 c=cases[r['index']];id=c['family']
 sig=re.search(r'inline FitResult fit_'+id+r'\((.*?)\) \{',(root/f'bindings/common/distributions/{id}/fit.hpp').read_text(),re.S)[1]
 integer='cppunivariate::ispan x' in sig
 print('{std::vector<'+('int' if integer else 'double')+'> x{'+','.join(str(int(v)) if integer else repr(v) for v in c['data'])+'};')
 call=['{x.data(),x.size()}']
 for arg in sig.split(',')[1:]:
  decl,default=arg.strip().split('=',1);typ,name=decl.strip().split();key=name.removesuffix('_arg')
  if typ!='Held':break
  call.append('{true,'+repr(c['held'][key])+',1}' if key in c['held'] else '{}')
 print('auto r=umvl_binding::fit_'+id+'('+','.join(call)+');')
 print('std::cout<<"{\\"index\\":'+str(c['index'])+',\\"id\\":\\"'+id+'\\",\\"estimates\\":{";bool first=true;for(auto v:r.estimates){if(!first)std::cout<<",";first=false;std::cout<<"\\\""<<v.first<<"\\\":";if(std::isfinite(v.second))std::cout<<v.second;else std::cout<<"null";}std::cout<<"},\\"error\\":\\""<<r.error<<"\\"}"<<std::endl;}')
print('}')
