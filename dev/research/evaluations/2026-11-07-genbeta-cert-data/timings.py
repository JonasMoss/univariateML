#!/usr/bin/env python3
"""Reproduce baseline/current timings and golden drift in one CPU slot.

Uses only existing C++/R/BH; no writes outside this worktree. The frozen
baseline is the job's starting mle.hpp. argv[1], if present, is output JSON.
"""
import json
from pathlib import Path
import re
import subprocess
import sys
root=Path(__file__).resolve().parents[4]
data=Path(__file__).resolve().parent
scratch=root/'build/scratch/genbeta-timings'
scratch.mkdir(parents=True,exist_ok=True)
family=root/'univariateML-cpp/include/cppunivariate/distributions/genbeta'
baseline=(data/'mle-baseline.hpp.txt').read_text()
baseline=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((family/m[1]).resolve())+'"',baseline)
(scratch/'baseline.hpp').write_text(baseline)
g=json.loads((family/'golden.json').read_text())
body=r'''
#include HEADER
#include <chrono>
#include <iostream>
int main(){
const std::vector<double> x{DATA};
std::cout.precision(17);
for(int route=0;route<4;++route){
  const auto begin=std::chrono::steady_clock::now();
  cppunivariate::GenbetaResult r{};
  for(int i=0;i<30;++i)r=cppunivariate::genbeta_mle({x.data(),x.size()},1,1e-7,300,
    route==0?NAN:route==2?1:2,route==1?.7:NAN);
  const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-begin).count()/30;
  std::cout<<route<<" "<<ms<<" "<<int(r.status)<<" "<<r.shape1<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.logLik<<"\n";
}}
'''.replace('DATA',','.join(map(repr,g['data'])))
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
results={}
for name,header in [('before',str(scratch/'baseline.hpp')),('after','cppunivariate/distributions/genbeta/mle.hpp')]:
    source=scratch/f'{name}.cpp'
    source.write_text(body.replace('HEADER','"'+header+'"'))
    executable=scratch/name
    subprocess.run(['c++','-std=c++17','-O2','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(source),'-o',str(executable)],check=True)
    rows=[]
    for line in subprocess.check_output([str(executable)],text=True).splitlines():
        route,ms,status,p,q,a,ll=map(float,line.split())
        assert status==0,line
        rows.append(dict(route=['default','power_only','held_p_one','held_p_two'][int(route)],milliseconds=ms,p=p,q=q,a=a,logLik=ll))
    results[name]=rows
for row in [results['before'][0],results['after'][0]]:
    for short,long in [('p','shape1'),('q','shape2'),('a','shape3')]:
        assert abs(row[short]/g['estimates'][long]-1)<1e-5,(short,row)
    assert abs(row['logLik']-g['logLik'])<1e-8
assert results['before'][0]['p']==results['after'][0]['p']
results['machine']=subprocess.check_output(['c++','--version'],text=True).splitlines()[0]
results['repetitions']=30
out=json.dumps(results,indent=2)+'\n'
if len(sys.argv)>1 and sys.argv[1]!='-':Path(sys.argv[1]).write_text(out)
else:print(out)
