"""Bitwise seeded-stream regression at all frozen truth sampler selectors."""
import json,re,subprocess,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE));import audit
D=audit.DISTS;WORK=audit.WORK

def main():
 calls={}
 for ident in audit.adapters():
  part=(D/ident/'test.cpp').read_text().split('mltest::check_dpqr_truth("'+ident+'",')[1]
  signature,call=re.findall(r'\[\]\(([^\n]*?)\)\s*\{\s*return ([^;]+);\s*\}',part)[3]
  names=[re.search(r'(\w+)\s*$',arg).group(1) for arg in signature.split(',')]
  mapping=dict(zip(names,['rng','p']))
  calls[ident]=re.sub(r'\b\w+\b',lambda m:mapping.get(m[0],m[0]),call)
 source='#include <iostream>\n#include <iomanip>\n#include <string>\n#include <vector>\n#include "cppunivariate/distributions/rng_std.hpp"\n'
 source+=''.join('#include "cppunivariate/distributions/'+ident+'/dpqr.hpp"\n' for ident in calls)
 source+='double sample(const std::string&id,cppunivariate::distributions::StdRng<>&rng,const std::vector<double>&p){\n'
 for ident,call in calls.items():source+='if(id=="'+ident+'")return '+call+';\n'
 source+='return NAN;}\nint main(){std::cout<<std::hexfloat;std::string id,s;unsigned seed;int n;while(std::cin>>id>>seed>>n){std::vector<double>p(n);for(double&v:p){std::cin>>s;v=std::strtod(s.c_str(),nullptr);}cppunivariate::distributions::StdRng<>rng(seed);for(int j=0;j<64;++j)std::cout<<sample(id,rng,p)<<"\\n";}}\n'
 src=WORK/'samplers.cpp';src.write_text(source)
 boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
 executables=[]
 for label,include in [('baseline',WORK/'baseline-source/univariateML-cpp/include'),('current',ROOT/'univariateML-cpp/include')]:
  exe=WORK/('samplers-'+label)
  if '--existing' not in sys.argv:subprocess.run(['c++','-std=c++17','-O1','-I'+str(include),'-I'+boost,str(src),'-o',str(exe)],check=True)
  executables.append(exe)
 jobs=[]
 for ident in calls:
  table=json.loads((D/ident/'truth.json').read_text())
  for at in table['sampler_points']:
   point=at[0];jobs.append(dict(family=ident,point=point,seed=20261029+point,parameters=table['parameters'][point]))
 for ident,parameters in [('trgamma',[.001,2.,1.]),('trgamma',[1e-8,1e8,1.]),('invtrgamma',[.001,2.,1.]),('invtrgamma',[1e-8,1e8,1.]),('trgamma',[1e-309,1e308,1.]),('invtrgamma',[1e-309,1e308,1.]),('gengamma',[0.,1.,2.]),('gengamma',[0.,1.,10.]),('gengamma',[0.,1.,-2.]),('gengamma',[0.,1.,-10.]),('tbinom',[2.**-1022,-1.,1.,2.**1022]),('tpois',[926275.886,0.,10.])]:
  jobs.append(dict(family=ident,point=None,source='shared-inverse fallback',seed=20261029,parameters=parameters))
 lines=[' '.join([j['family'],str(j['seed']),str(len(j['parameters'])),*map(str,j['parameters'])]) for j in jobs]
 outputs=[subprocess.check_output([str(exe)],input='\n'.join(lines)+'\n',text=True).splitlines() for exe in executables]
 assert all(len(o)==64*len(jobs) for o in outputs)
 changes=[]
 for k,j in enumerate(jobs):
  a,b=(o[64*k:64*(k+1)] for o in outputs)
  if a!=b:changes.append(dict(j,changed=sum(x!=y for x,y in zip(a,b)),baseline=a,current=b))
 result=dict(families=len(calls),selectors=len(jobs),draws=64*len(jobs),changes=changes)
 (HERE/'sampler-streams.json').write_text(json.dumps(result,indent=2)+'\n')
 print('sampler streams',result['draws'],'draws;',len(changes),'changed selectors',flush=True)
if __name__=='__main__':main()
