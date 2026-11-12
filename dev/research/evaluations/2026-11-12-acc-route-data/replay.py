#!/usr/bin/env python3
"""Run under /tmp/umvl-cpu/run. Rebuild baseline read-only; compare successful fits bitwise."""
import json
import hashlib
from pathlib import Path
import subprocess
import tarfile

ROOT=Path(__file__).resolve().parents[4]
BASE='9e4a368577f752f3387ece249203fc40f7e022ca'
SCRATCH=ROOT/'build/scratch/acc-route-replay'
SCRATCH.mkdir(parents=True,exist_ok=True)
base=SCRATCH/'baseline'
base.mkdir(exist_ok=True)
archive=subprocess.Popen(['git','archive',BASE,'univariateML-cpp/include'],cwd=ROOT,stdout=subprocess.PIPE)
with tarfile.open(fileobj=archive.stdout,mode='r|') as stream:
    stream.extractall(base,filter='data')
if archive.wait():
    raise SystemExit('baseline extraction failed')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
source=['#include "cppunivariate/distributions/gengamma/mle.hpp"',
        '#include "cppunivariate/distributions/cmb/mle.hpp"',
        '#include "cppunivariate/distributions/dgamma/mle.hpp"',
        '#include <iostream>','#include <iomanip>','int main(){std::cout<<std::hexfloat;']
for family,names in [('gengamma',['mu','sigma','Q']),('cmb',['prob','nu']),('dgamma',['shape','rate'])]:
    g=json.loads((ROOT/'univariateML-cpp/include/cppunivariate/distributions'/family/'golden.json').read_text())
    values=','.join(map(str,g['data']))
    source.append('{std::vector<double> data{'+values+'};')
    if family=='cmb':
        source.append('std::vector<int> counts;for(double v:data)counts.push_back(int(v));')
    for mask in range(1<<len(names)):
        held=[repr(g['estimates'][name]) if mask&(1<<i) else 'NAN' for i,name in enumerate(names)]
        call={'gengamma':'gengamma_mle({data.data(),data.size()},1e-10,100,',
              'cmb':'cmb_mle({counts.data(),counts.size()},10,1e-10,2000,',
              'dgamma':'dgamma_mle({data.data(),data.size()},1e-10,400,'}[family]+','.join(held)+')'
        source.append('{const auto r=cppunivariate::'+call+';std::cout<<"'+family+' '+str(mask)+' "<<int(r.status);')
        source.append('if(int(r.status)==0)std::cout'+''.join('<<" "<<r.'+name for name in names)+'<<" "<<r.logLik<<" "<<r.iterations'+('<<" "<<r.certified' if family=='gengamma' else '')+';std::cout<<"\\n";}')
    source.append('}')
source.append('}')
cpp=SCRATCH/'replay.cpp';cpp.write_text('\n'.join(source)+'\n')
results={}
for mode,flags in [('extended',[]),('double',['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])]:
    for name,include in [('baseline',base/'univariateML-cpp/include'),('current',ROOT/'univariateML-cpp/include')]:
        binary=SCRATCH/(name+'-'+mode)
        subprocess.run(['c++','-std=c++17','-O2',*flags,'-I'+str(include),'-I'+boost,str(cpp),'-o',str(binary)],check=True)
        results[name,mode]=subprocess.check_output([str(binary)],text=True)
    if results['baseline',mode]!=results['current',mode]:
        (SCRATCH/(mode+'-baseline.txt')).write_text(results['baseline',mode])
        (SCRATCH/(mode+'-current.txt')).write_text(results['current',mode])
        raise SystemExit('fit drift in '+mode+'; inspect '+str(SCRATCH))
summary={'baseline':BASE,'compiler':subprocess.check_output(['c++','--version'],text=True).splitlines()[0],'modes':{mode:{'fits':len(results['current',mode].splitlines()),
    'successful':sum(line.split()[2]=='0' for line in results['current',mode].splitlines()),
    'bitwise_equal':True,'output_sha256':hashlib.sha256(results['current',mode].encode()).hexdigest()} for mode in ['extended','double']}}
output=Path(__file__).with_name('replay.json');output.write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
