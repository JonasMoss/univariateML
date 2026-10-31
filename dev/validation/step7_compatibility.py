#!/usr/bin/env python3
"""Byte-compare existing affected truth evaluations before/after header move.
Run through /tmp/umvl-cpu/run. Uses a read-only git archive of HEAD in scratch.
"""
from pathlib import Path
import hashlib, io, json, math, subprocess, tarfile
from numerical_inventory import INC, ROOT, closure
OUT=ROOT/'build/scratch/step7-compatibility';OUT.mkdir(parents=True,exist_ok=True)
base=OUT/'baseline';base.mkdir(exist_ok=True)
data=subprocess.check_output(['git','archive','HEAD','univariateML-cpp/include'],cwd=ROOT)
with tarfile.open(fileobj=io.BytesIO(data)) as archive:
    archive.extractall(base,filter='data')
families=[]
for p in sorted((INC/'distributions').glob('*/dpqr.hpp')):
    if (INC/'special/beta_tails.hpp').resolve() in closure(p):families.append(p.parent.name)
def literal(x):
    x=float(x)
    if math.isnan(x):return 'std::numeric_limits<double>::quiet_NaN()'
    if math.isinf(x):return ('-' if x<0 else '')+'std::numeric_limits<double>::infinity()'
    return x.hex()
source=['#include <iostream>','#include <iomanip>','#include <limits>']
for f in families:source.append(f'#include "cppunivariate/distributions/{f}/dpqr.hpp"')
source += ['int main() {','std::cout<<std::hexfloat;']
counts={}
for f in families:
    t=json.loads((INC/f'distributions/{f}/truth.json').read_text());params=t['parameters']
    # Arrays keep compilation compact despite thousands of fixture rows.
    source.append('{')
    source.append(f'const double params[][{len(params[0])}]='+'{{'+ '},{'.join(','.join(literal(x) for x in p) for p in params)+'}};')
    source.append('const double values[][2]={{'+ '},{'.join(f'{r[0]},{literal(r[1])}' for r in t['values'])+'}};')
    args=','.join(f'p[{j}]' for j in range(len(params[0])))
    source.append('for(const auto &r:values) {const auto &p=params[int(r[0])];')
    source.append(f'std::cout<<cppunivariate::distributions::d{f}(r[1],{args},true)<<"\\n";')
    source.append(f'for(bool lower:{{false,true}})for(bool log:{{false,true}})std::cout<<cppunivariate::distributions::p{f}(r[1],{args},lower,log)<<"\\n";')
    source.append('}')
    source.append('const double quantiles[][4]={{'+'},{'.join(','.join(literal(x) for x in r[:4]) for r in t['quantiles'])+'}};')
    source.append(f'for(const auto &r:quantiles){{const auto &p=params[int(r[0])];std::cout<<cppunivariate::distributions::q{f}(r[3],{args},bool(r[1]),bool(r[2]))<<"\\n";}}')
    source.append('}')
    counts[f]=len(t['values'])*5+len(t['quantiles'])
source.append('}')
(OUT/'probe.cpp').write_text('\n'.join(source))
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
outputs=[]
for label,inc in [('before',base/'univariateML-cpp/include'),('after',ROOT/'univariateML-cpp/include')]:
    subprocess.run(['g++','-fmax-errors=3','-O1','-std=c++17','-I'+str(inc),'-I'+boost,str(OUT/'probe.cpp'),'-o',str(OUT/label)],check=True)
    result=subprocess.check_output([str(OUT/label)])
    (OUT/(label+'.txt')).write_bytes(result);outputs.append(result)
assert outputs[0]==outputs[1], 'header move changed truth evaluations'
summary={'families':counts,'evaluations':sum(counts.values()),'byte_identical':True,'sha256':hashlib.sha256(outputs[0]).hexdigest(),'revision':subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()}
(OUT/'summary.json').write_text(json.dumps(summary,indent=2)+'\n');print(json.dumps(summary,indent=2))
