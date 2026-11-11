"""Exact binary64 high-precision gamma-deviance checks, including subnormals."""
import json,math,sys
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
sys.path.insert(0,str(HERE));import audit
mp.mp.dps=1600
jobs=[];expected=[]
def add(ident,x,pars):
 if not (x>0 and math.isfinite(x)):return
 v=mp.mpf(x);a,b=map(mp.mpf,pars)
 if ident=='gamma':ld=-mp.loggamma(a)-a*mp.log(b)+(a-1)*mp.log(v)-v/b
 elif ident=='invgamma':ld=a*mp.log(b)-mp.loggamma(a)-(a+1)*mp.log(v)-b/v
 else:ld=mp.log(2)+a*mp.log(a/b)-mp.loggamma(a)+(2*a-1)*mp.log(v)-a*v*v/b
 jobs.append((ident,0,x,True,True,pars));expected.append(float(ld))
for shape in (16.,1e8,1e24,1e308):
 for scale in (1000.,1e-300,float.fromhex('0x0.0000000000001p-1022'),1e300):
  center=float(mp.mpf(shape)*mp.mpf(scale))
  if math.isfinite(center):
   for x in (center,math.nextafter(center,0),math.nextafter(center,math.inf)):add('gamma',x,[shape,scale])
 for beta in (1.,1e8,1e308):
  center=float(mp.mpf(beta)/mp.mpf(shape))
  for x in (center,math.nextafter(center,0),math.nextafter(center,math.inf)):add('invgamma',x,[shape,beta])
 for scale in (1000.,1e-300,1e300):
  center=math.sqrt(scale)
  for x in (center,math.nextafter(center,0),math.nextafter(center,math.inf)):add('naka',x,[shape,scale])
rows=[]
for job,a,b in zip(jobs,audit.evaluate(jobs),expected):rows.append(dict(job=job,actual=a,expected=b,error=audit.error(a,b)))
(HERE/'density-smoke.json').write_text(json.dumps(rows,indent=2)+'\n')
print('gamma deviance smoke',len(rows),'worst',max(r['error'] for r in rows),flush=True)
for r in rows:
 if r['error']>1e-10:print('FAILED',r,flush=True)
assert all(r['error']<=1e-10 for r in rows)
for ident in ('gamma','invgamma','naka'):
 candidates=[r for r in rows if r['job'][0]==ident and r['job'][5][0]==1e8 and r['job'][5][1] in (1000.,1e8)]
 r=candidates[0];_,_,x,_,_,pars=r['job'];path=ROOT/'univariateML-cpp/include/cppunivariate/distributions'/ident/'test.cpp'
 title=ident+': compensated concentrating density'
 if title in path.read_text():continue
 path.write_text(path.read_text()+'\n// Independent 1600-digit reference; density-smoke.py retains exact inputs.\nTEST_CASE("'+title+'") {\n  CHECK(cppunivariate::distributions::d'+ident+'('+','.join(map(repr,[x,*pars]))+',true)==\n        doctest::Approx('+repr(r['expected'])+').epsilon(1e-10).scale(0));\n}\n')
