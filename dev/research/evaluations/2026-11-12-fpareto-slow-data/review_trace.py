from pathlib import Path
import re,json,gzip,subprocess,os
root=Path.cwd();p=root/'build/scratch/fpareto-review';family=root/'univariateML-cpp/include/cppunivariate/distributions/gb2'
source=p/'gb2-before.hpp' if (p/'gb2-before.hpp').exists() else p/'pre-review-gb2.hpp'
s=source.read_text();s=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((family/m[1]).resolve())+'"',s)
s='#include <cstdio>\n'+s
s=s.replace('if(local.exhausted)return cur;', '''if(local.exhausted){std::fprintf(stderr,"LOCAL used=%lld ok=%d lp=%g lq=%g g=(%g,%g) ll=%.17g box=%g\\n",static_cast<long long>(local.used),cur.ok,std::log(cur.p),std::log(cur.q),cur.g[0],cur.g[1],cur.ll,box);return cur;}''')
s=s.replace('      if(candidate.ok) {','      std::fprintf(stderr,"GRID used=%lld done=%d ok=%d lp=%g lq=%g ll=%.17g\\n",static_cast<long long>(work.used),candidate.outer_done,candidate.ok,std::log(candidate.p),std::log(candidate.q),candidate.ll);\n      if(candidate.ok) {',1)
s=s.replace('    if(work.exhausted)break;\n    const bool edge=', '    std::fprintf(stderr,"BOX %g used=%lld exhausted=%d explored=%d face_failure=%d bestLL=%.17g face=%.17g fs=%d\\n",box,static_cast<long long>(work.used),work.exhausted,explored,face_failure,best.ll,face,int(fs));\n    if(work.exhausted)break;\n    const bool edge=')
(p/'trace.hpp').write_text(s)
src='#include "trace.hpp"\n#include <iostream>\nint main(int argc,char**argv){int id=std::atoi(argv[1]);\n'
cases=json.load(gzip.open('dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz'))
for id in [1601,10072]:
 c=next(c for c in cases if c['index']==id);src+=f'if(id=={id}){{std::vector<double>x{{'+','.join(map(str,c['data']))+'};auto r=cppunivariate::gb2_mle({x.data(),x.size()},1e-7,1000);std::cout<<int(r.status)<<" "<<r.logLik<<std::endl;}\n'
src+='}\n';(p/'trace.cpp').write_text(src)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
subprocess.run(['g++','-O2','-std=c++17','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(p/'trace.cpp'),'-o',str(p/'trace')],check=True)
for id in [1601,10072]:
 with (p/f'trace-{id}.log').open('w') as f:subprocess.run([str(p/'trace'),str(id)],stdout=f,stderr=subprocess.STDOUT,check=True)
