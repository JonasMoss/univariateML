"""Exact archived inputs, frozen-reference replay and before/after fit preservation."""
import json,gzip,sys,subprocess,math,collections,os
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]; H=Path(__file__).resolve().parent; W=ROOT/'build/scratch/dpq-skew'; W.mkdir(exist_ok=True)
F=('ged','sged','sstd','std','st');D=ROOT/'univariateML-cpp/include/cppunivariate/distributions'
def build():
 include=ROOT/'univariateML-cpp/include';prefix='probe'
 if os.environ.get('DPQ_SKEW_BASELINE')=='1':
  checkout=W/'baseline-source';checkout.mkdir(exist_ok=True)
  archive=subprocess.Popen(['git','archive','217ab8b533cac25ccc8eba8d66eccde1b376f0b4','univariateML-cpp/include'],cwd=ROOT,stdout=subprocess.PIPE)
  subprocess.run(['tar','-x','-C',str(checkout)],stdin=archive.stdout,check=True)
  archive.stdout.close();assert archive.wait()==0
  include=checkout/'univariateML-cpp/include';prefix='probe-baseline'

 src='#include <iostream>\n#include <iomanip>\n#include <vector>\n#include <string>\n'
 src+=''.join('#include "cppunivariate/distributions/'+f+'/dpqr.hpp"\n#include "cppunivariate/distributions/'+f+'/mle.hpp"\n' for f in F)
 src+='int main(){std::cout<<std::setprecision(17);std::string f,s;int op,n,lo,lg;double x;while(std::cin>>f>>op>>s>>lo>>lg>>n){x=std::stod(s);std::vector<double> p(n);for(auto&v:p){std::cin>>s;v=std::stod(s);}double a=NAN;'
 for f in F:
  pars=','.join('p['+str(i)+']' for i in range(3 if f in ('ged','std') else 4))
  src+=f'if(f=="{f}"){{if(op==0)a=cppunivariate::distributions::d{f}(x,{pars},lg);if(op==1)a=cppunivariate::distributions::p{f}(x,{pars},lo,lg);if(op==2)a=cppunivariate::distributions::q{f}(x,{pars},lo,lg);'
  fields=['xi','omega','alpha','nu'] if f=='st' else ['mu','sigma','nu']+(['xi'] if f in ('sged','sstd') else [])
  fn='student_t_mle' if f=='std' else f+'_mle'
  nc=len(fields); holds=','.join('p['+str(i)+']' for i in range(nc)); src+=f'if(op==3){{std::vector<double> data(p.begin()+{nc},p.end());auto r=cppunivariate::{fn}(cppunivariate::span{{data.data(),data.size()}},1e-10,200,{holds});std::cout<<int(r.status)<<" "<<r.logLik'
  src+=''.join('<<" "<<r.'+v for v in fields)+'<<"\\n";continue;}}'
 src+='std::cout<<a<<"\\n";}}'
 (W/'probe.cpp').write_text(src)
 boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
 for mode,flags in [('native',[]),('double',['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])]:
  subprocess.run(['c++','-std=c++17','-O2',*flags,'-I'+str(include),'-I'+boost,str(W/'probe.cpp'),'-o',str(W/(prefix+'-'+mode))],check=True)
def evaluate(jobs,mode,kind="probe"):
 import os
 kind=os.environ.get("DPQ_SKEW_BINARY",kind)
 def fmt(x):return str(x).replace('Inf','inf')
 lines=[' '.join([f,str(op),fmt(x),str(int(lo)),str(int(lg)),str(len(p)),*map(fmt,p)]) for f,op,x,lo,lg,p in jobs]
 return subprocess.check_output([str(W/(kind+'-'+mode))],input='\n'.join(lines)+'\n',text=True,timeout=120).splitlines()
def replay(label):
 cases=json.load(gzip.open(ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz','rt'));jobs=[];ids=[]
 for f in F:
  g=json.load(open(D/f/'golden.json')); jobs.append((f,3,0,1,1,[math.nan]*(3 if f in ('ged','std') else 4)+g['data']));ids.append([f,'golden'])
 for c in cases:
  if c['family'] not in F:continue
  f=c['family'];t=json.load(open(D/f/'truth.json')); names=t['parameter_names'];jobs.append((f,3,0,1,1,[c['held'].get(n,math.nan) for n in names]+c['data']));ids.append([f,c['index']])
 # One process per case makes a slow legacy fit an explicit record, not a blocked inventory.
 out=[]
 for i,(job,ident) in enumerate(zip(jobs,ids)):
  try:r=evaluate([job],os.environ.get('DPQ_SKEW_FIT_MODE','native'))[0]
  except subprocess.TimeoutExpired:r='timeout'
  out.append(dict(id=ident,result=r))
  if i%100==0:print(label,i,len(jobs),flush=True)
 (H/(label+'-fits.json')).write_text(json.dumps(out,indent=2)+'\n')
if __name__=='__main__':
 if sys.argv[1]=='build':build()
 elif sys.argv[1]=='fits':replay(sys.argv[2])
