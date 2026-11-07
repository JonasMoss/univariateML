"""Compile and time frozen baseline and working sources on identical golden data.
Invoke through /tmp/umvl-cpu/run from the worktree root; outputs JSON.
"""
import json
from pathlib import Path
import subprocess
base_revision='017b9f8ec3bed7610ccd41c62738bce1ea987316'
root=Path.cwd(); data=root/'dev/research/evaluations/2026-11-07-scalar-trio-data';scratch=root/'build/scratch'
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
prefix='#include <chrono>\n#include <iostream>\n#include <iomanip>\n#include <vector>\n'
base='univariateML-cpp/include/cppunivariate/distributions'
body='int main(){std::cout<<std::setprecision(17);const double nan=std::numeric_limits<double>::quiet_NaN();\n'
for f in ('fatigue','sgomp','genbeta'):
    g=json.loads((root/base/f/'golden.json').read_text())
    body+=' {std::vector<double> x{'+','.join(map(repr,g['data']))+'};\n'
    exprs={'fatigue':['fatigue_mle({x.data(),x.size()},1.220703e-4,100)','fatigue_mle({x.data(),x.size()},1e-10,200,3)','fatigue_mle({x.data(),x.size()},1e-10,200,nan,2)'],
           'sgomp':['sgomp_mle({x.data(),x.size()})','sgomp_mle({x.data(),x.size()},1e-10,200,nan,2)','sgomp_mle({x.data(),x.size()},1e-10,200,1)'],
           'genbeta':['genbeta_mle({x.data(),x.size()},1)','genbeta_mle({x.data(),x.size()},1,1e-7,300,2,.7)','genbeta_mle({x.data(),x.size()},1,1e-7,300,2,2)']}[f]
    for i,expr in enumerate(exprs):
        fields={'fatigue':'<<r.alpha<<" "<<r.beta','sgomp':'<<r.b<<" "<<r.eta','genbeta':'<<r.shape1<<" "<<r.shape2<<" "<<r.shape3'}[f]
        body+=' {auto start=std::chrono::steady_clock::now();\n'
        body+=f'for(int j=0;j<20;++j){{auto r=cppunivariate::{expr};if(j==0)std::cout<<"{f} {i} "<<int(r.status)<<" "{fields}<<" "<<r.logLik<<" ";}}\n'
        body+='std::cout<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()/20<<"\\n";}\n'
    body+='}\n'
body+='}\n'
results={}
for mode in ('before','after'):
    source=prefix
    for f in ('fatigue','sgomp','genbeta'):
        path=Path(base)/f/'mle.hpp'
        if mode=='after':source+=f'#include "{root/path}"\n'
        else:
            text=subprocess.check_output(['git','show',base_revision+':'+str(path)],text=True)
            import re
            text=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((root/path.parent/m[1]).resolve())+'"',text)
            source+=text+'\n'
    src=scratch/f'timings-{mode}.cpp';binary=scratch/f'timings-{mode}';src.write_text(source+body)
    subprocess.run(['c++','-std=c++17','-O2','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(src),'-o',str(binary)],check=True,capture_output=True)
    rows=[]
    for line in subprocess.check_output([str(binary)],text=True).splitlines():
        r=line.split();rows.append({'family':r[0],'workload':int(r[1]),'status':int(r[2]),'coef':list(map(float,r[3:-2])),'logLik':float(r[-2]),'milliseconds_per_fit':float(r[-1])})
    results[mode]=rows
results['repeats']=20
results['workloads']=['default','held alpha=3 / eta=2 / beta shapes=2,.7','held beta=2 / rate=1 / beta shapes=2,2']
results['compiler']=subprocess.check_output(['c++','--version'],text=True).splitlines()[0]
results['base_revision']=base_revision
import hashlib
results['working_headers_sha256']={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in [Path(base)/f/'mle.hpp' for f in ('fatigue','sgomp','genbeta')]+[Path(base)/'fatigue/certificate.hpp']}
print(json.dumps(results,indent=2))
