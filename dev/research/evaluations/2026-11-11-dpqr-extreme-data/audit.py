"""Standalone all-row forward audit; adapters are taken from family truth tests.

Build: audit.py build. Compare: audit.py check LABEL. Seam: audit.py seam.
"""
import csv,gzip,json,math,re,subprocess,sys,os
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]; HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(ROOT/'dev/validation'))
from quality.truth import DISTS,read
WORK=ROOT/'build/scratch/dpqr-extreme';WORK.mkdir(exist_ok=True)
EXE=WORK/'probe'
def adapters():
 out={}
 for folder in sorted(DISTS.iterdir()):
  if not (folder/'test.cpp').exists():continue
  text=(folder/'test.cpp').read_text()
  part=text.split('mltest::check_dpqr_truth("'+folder.name+'",')
  if len(part)<2:continue
  matches=re.findall(r'\[\]\(([^\n]*?)\)\s*\{\s*return ([^;]+);\s*\}',part[1])[:3]
  calls=[]
  for index,(signature,call) in enumerate(matches):
   names=[re.search(r'(\w+)\s*$',arg).group(1) for arg in signature.split(',')]
   canonical=['x','p','log'] if index==0 else ['x','p','lower','log']
   mapping=dict(zip(names,canonical))
   calls.append(re.sub(r'\b\w+\b',lambda m:mapping.get(m[0],m[0]),call))
  if len(calls)==3:out[folder.name]=calls
 return out

def build():
 ads=adapters()
 baseline=len(sys.argv)<3 or sys.argv[2]=='baseline'
 src='#include <iostream>\n#include <iomanip>\n#include <vector>\n#include <string>\n#include <cmath>\n'
 src+=''.join('#include "cppunivariate/distributions/'+i+'/dpqr.hpp"\n' for i in ads)
 include=ROOT/'univariateML-cpp/include'
 target=EXE
 if baseline:
  checkout=WORK/'baseline-source';checkout.mkdir(exist_ok=True)
  if os.environ.get('UMVL_AUDIT_BASELINE_READY')!='1':
   archive=subprocess.Popen(['git','archive','HEAD','univariateML-cpp/include'],cwd=ROOT,stdout=subprocess.PIPE)
   subprocess.run(['tar','-x','-C',str(checkout)],stdin=archive.stdout,check=True)
   archive.stdout.close();assert archive.wait()==0
  else:assert (checkout/'univariateML-cpp/include/cppunivariate/special/tails.hpp').exists()
  include=checkout/'univariateML-cpp/include'
  target=WORK/'probe-baseline'
 src+='double eval(const std::string&id,int mode,double x,const std::vector<double>&p,bool lower,bool log){\n'
 for ident,calls in ads.items():
  src+='if(id=="'+ident+'") {switch(mode){'+''.join('case '+str(i)+':return '+call+';' for i,call in enumerate(calls))+'}}\n'
 src+='return NAN;}\nint main(){std::cout<<std::setprecision(17);std::string id,xs;int mode,lo,lg,n;while(std::cin>>id>>mode>>xs>>lo>>lg>>n){double x=std::strtod(xs.c_str(),nullptr);std::vector<double>p(n);for(double&v:p){std::cin>>xs;v=std::strtod(xs.c_str(),nullptr);}try{std::cout<<eval(id,mode,x,p,lo,lg)<<"\\n";}catch(...){std::cout<<"nan\\n";}}}\n'
 source=WORK/('probe-baseline.cpp' if baseline else 'probe.cpp')
 source.write_text(src)
 boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
 flags=[]
 if len(sys.argv)>3 and sys.argv[3]=='no-long-double':
  flags=['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'];target=WORK/'probe-double'
 subprocess.run(['c++',*flags,'-std=c++17','-O1','-I'+str(include),'-I'+boost,str(source),'-o',str(target)],check=True)
 (HERE/'adapters.json').write_text(json.dumps(ads,indent=2)+'\n')

def num(v):return float(v)
def fmt(v):return str(v).replace('Inf','inf')
def evaluate(jobs):
 lines=[' '.join([i,str(m),fmt(x),str(int(lo)),str(int(lg)),str(len(p)),*map(fmt,p)]) for i,m,x,lo,lg,p in jobs]
 result=subprocess.run([str(EXE)],input='\n'.join(lines)+'\n',text=True,capture_output=True,check=True)
 vals=list(map(float,result.stdout.splitlines()));assert len(vals)==len(jobs),(len(vals),len(jobs),result.stderr)
 return vals

def error(a,b,relative=False):
 if a==b:return 0.
 if not math.isfinite(a) or not math.isfinite(b):return math.inf
 return abs(a-b)/(abs(b) if relative and b else max(1,abs(b)))

def check(label):
 global EXE
 if label=='before': EXE=WORK/'probe-baseline'
 elif label.endswith('-double'): EXE=WORK/'probe-double'
 jobs=[]; refs=[]
 for ident in adapters():
  path=HERE/'measured-tables'/(ident+'.json')
  table=json.loads((path if path.exists() else DISTS/ident/'truth.json').read_text()); ps=table['parameters']
  for entry in table.get('excluded_parameter_points',[]):
   index=len(ps);ps.append(entry['parameters'])
   for category in ('values','quantiles'):
    table.setdefault('excluded_'+category,[]).extend(dict(row=[index,*r[1:]],reason=entry['reason']) for r in entry[category])
  value_rows=[(r,None) for r in table['values']]+[(r['row'],r['reason']) for r in table.get('excluded_values',[])]
  for row,reason in value_rows:
   p=ps[row[0]];x=row[1]
   for mode,lo,lg,b in [(0,True,True,row[2]),(1,True,True,row[3]),(1,False,True,row[4])]:
    jobs.append((ident,mode,x,lo,lg,p));refs.append((num(b),False,1e-10,None,row,reason))
   for lo,b in [(True,row[3]),(False,row[4])]:
    b=math.exp(num(b))
    if b>1e-300:jobs.append((ident,1,x,lo,False,p));refs.append((b,True,1e-10,None,row,reason))
  quantile_rows=[(r,None) for r in table['quantiles']]+[(r['row'],r['reason']) for r in table.get('excluded_quantiles',[])]
  for row,reason in quantile_rows:
   jobs.append((ident,2,row[3],row[1],row[2],ps[row[0]]));refs.append((num(row[4]),True,1e-9,row[5:9],row,reason))
 actual=evaluate(jobs)
 discrete={i: read(DISTS/i)[0]['support']['type']=='integer' for i in adapters()}
 lattice_jobs=[];lattice_indices=[]
 for index,(job,a,ref) in enumerate(zip(jobs,actual,refs)):
  ident,mode,x,lo,lg,parameters=job
  b=ref[0];inp=num(x)
  interior=math.isfinite(inp) and (inp<0 if lg else 0<inp<1)
  if mode==2 and discrete[ident] and abs(b)>2**53 and interior:
   lattice_indices.append(index)
   lattice_jobs.extend([(ident,1,a,lo,lg,parameters),(ident,1,math.nextafter(a,-math.inf),lo,lg,parameters)])
 lattice_values=iter(evaluate(lattice_jobs)) if lattice_jobs else iter(())
 lattice={}
 for index in lattice_indices:
  at,before=next(lattice_values),next(lattice_values)
  inp=num(jobs[index][2]);lo=jobs[index][3]
  lattice[index]=(at>=inp and before<inp) if lo else (at<=inp and before>inp)
 failures=[];counts={}
 for index,(job,a,(b,rel,tol,bracket,row,reason)) in enumerate(zip(jobs,actual,refs)):
  e=error(a,b,rel); ok=e<=tol
  if bracket:
   l,h,amb,floor=map(num,bracket)
   ok=ok or (a==l or a==h if amb else l<=a<=h) or (math.isfinite(a) and math.isfinite(b) and abs(a-b)<=floor)
  if index in lattice: ok=ok and lattice[index]
  if not ok:
   ident,mode,x,lo,lg,p=job
   f=dict(family=ident,function=['d','p','q'][mode],input=x,lower=bool(lo),log=bool(lg),parameters=p,actual=a,expected=b,error=e,category='quantiles' if mode==2 else 'values',row=row,excluded_reason=reason)
   failures.append(f);counts[ident]=counts.get(ident,0)+1
 (HERE/(label+'-failures.json')).write_text(json.dumps(failures,indent=2)+'\n')
 summary=dict(checks=len(jobs),failures=len(failures),active_failures=sum(r['excluded_reason'] is None for r in failures),ranked=sorted(counts.items(),key=lambda p:-p[1]))
 (HERE/(label+'-summary.json')).write_text(json.dumps(summary,indent=2)+'\n');print(summary)

if __name__=='__main__':
 if sys.argv[1]=='build':build()
 elif sys.argv[1]=='check':check(sys.argv[2])
