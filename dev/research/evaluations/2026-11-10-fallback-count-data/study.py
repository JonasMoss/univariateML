#!/usr/bin/env python3
"""Frozen audit stress cases plus seeded route probes; replay against two binaries."""
import collections,gzip,json,pathlib,random,subprocess,sys,os
HERE=pathlib.Path(__file__).resolve().parent
ROOT=HERE.parents[3]
FAMILIES={'bbinom','hyper','nhyper','tbinom','ztbinom','zib','zmbinom'}
def generate():
 # The Nov 9 rerun uses the frozen Nov 7 inventory (audit.py models()).
 cases=[dict(c,source='2026-11-09 audit replay') for c in json.load(gzip.open(ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz','rt')) if c['family'] in FAMILIES]
 rng=random.Random(1681)
 def add(f,x,held,label):cases.append(dict(family=f,data=x,held=held,sample=label,source='seed 1681'))
 for j in range(12):
  x=[rng.randrange(5) for _ in range(16)]
  add('bbinom',x,dict(size=4),'fixed-size');add('bbinom',x,dict(alpha=1),'held-alpha');add('bbinom',x,dict(beta=1e9),'held-beta-preflight')
  add('hyper',x,{},'all-free');add('hyper',x,dict(k=4),'held-k')
  add('nhyper',[v+1 for v in x],dict(r=1),'growing-urn')
  add('tbinom',x,dict(a=-1,b=4),'finite-window');add('ztbinom',[v+1 for v in x],{},'positive-profile')
  add('zib',x,dict(pi=.3772146069463776),'held-inflation');add('zmbinom',x,{},'modified-profile')
 for f,x,h in [
  ('bbinom',[0,0,0,10],dict(alpha=1)),('bbinom',[0,1,2,4,8],dict(beta=1e9)),
  ('hyper',[8,8,9,9],{}),('hyper',[0,0,0,5,5,5],dict(k=5)),('hyper',[0,1],dict(N=10000)),
  ('nhyper',[1,1,1,1,1,30],dict(r=1)),('nhyper',[1,2],dict(N=1e7)),
  ('tbinom',[0,0,1,1,2,4],dict(a=-1,b=4)),
  ('zib',[0]*5+[1,1,2,2,4],dict(pi=.3772146069463776)),
  ('zib',[0]*5+[1,1,2,2,4],dict(pi=.37721460694637765))]:add(f,x,h,'route-regression')
 for f,x,h in [('ztbinom',[1,2,3],dict(prob=1e-9)),('zib',[0,1,2,3],dict(prob=1e-9)),('zmbinom',[0,1,2,3],dict(prob=1e-9))]:
  add(f,x,h,'held-probability-budget');cases[-1]['maxit']=1
 add('tbinom',[1,1,1,1,1,12],dict(a=0,b=1e100),'certificate-window')
 for h in [dict(size=10),dict(size=10,alpha=2),dict(size=10,beta=3)]:
  add('bbinom',[0,1,1,2,2,3,4,5,7,10],h,'coefficient-inclusion');cases[-1]['tol']=1e-18
 add('zib',[0]*17+[1]*2+[2]*15,dict(size=2,pi=.2),'zero-turning-discriminant')
 add('zib',[0]*34000+[1]*4000+[2]*30000,dict(size=2,pi=.2),'unclosed-turning-discriminant')
 add('nhyper',[2,2],dict(m=9007199254740991),'representable-stopping-count')
 add('tbinom',[2000001,2000002],dict(a=2000000,b=2000005),'attained-two-atom-large-count')
 for f,x in [('ztbinom',[1,1,1]),('zmbinom',[0,1,1])]:
  add(f,x,dict(prob=1e-9),'attained-one-positive-atom');cases[-1]['maxit']=1
 add('zib',[0]*17+[1]*2+[2]*15,dict(size=2,pi=54/343),'flat-probability-root-inclusion')
 for f,x,h,it in [('ztbinom',[1,2],{},200),('zmbinom',[0,1,1],dict(prob=1e-9),1),('zib',[1,1,2],dict(size=2,pi=0),200)]:
  add(f,x,h,'sub-machine-coefficient-target');cases[-1].update(tol=1e-18,maxit=it)
 add('zib',[0,1,1],dict(size=1,prob=1),'sub-machine-coefficient-target');cases[-1]['tol']=1e-18
 for i,c in enumerate(cases):c['study_index']=i
 (HERE/'cases.json').write_text(json.dumps(cases,indent=2))
 print(len(cases),dict(collections.Counter(c['family'] for c in cases)))
def run(binary,out):
 cases=json.loads((HERE/'cases.json').read_text());dest=HERE/out
 completed={int(line.split('\t')[0]) for line in dest.read_text().splitlines()} if dest.exists() else set()
 with dest.open('a') as stream:
  for c in cases:
   if c['study_index'] in completed:continue
   held=c['held'];vals=[str(held.get(k,'NA')) for k in ['size','alpha','beta','m','N','k','r','prob','pi','p0','a','b']]
   line=' '.join(map(str,[c['study_index'],c['family'],len(c['data']),*c['data'],*vals,c.get('maxit',500 if c['family']=='bbinom' else 200),c.get('tol',1e-10)]))+'\n'
   try:
    result=subprocess.run([binary],input=line,text=True,capture_output=True,timeout=int(os.environ.get("COUNT_TIMEOUT","60")),check=True)
    stream.write(result.stdout)
   except subprocess.TimeoutExpired:stream.write(f"{c['study_index']}\t{c['family']}\tstop:wall-guard\tnan\n")
   stream.flush()
   if c['study_index']%50==0:print(c['study_index'],flush=True)
if __name__=='__main__':
 if sys.argv[1]=='generate':generate()
 else:run(sys.argv[1],sys.argv[2])
