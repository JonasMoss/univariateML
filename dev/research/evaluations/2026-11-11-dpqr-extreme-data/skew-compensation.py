"""Exact-input high-precision checks around skew-normal's amplified centering seam."""
import json,math,sys
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
sys.path.insert(0,str(HERE));import audit
from quality.truth_reference import normal,pair
mp.mp.dps=200
jobs=[];expected=[]
moment=mp.sqrt(2/mp.pi)
for xi in (1e-12,1e-8,1e-4,1.,1e4,1e8,1e12):
 t=mp.mpf(xi);mu=moment*(t-1/t);sigma=mp.sqrt((1-moment**2)*(t*t+1/t**2)+2*moment**2-1)
 for mean,sd in ((0.,1.),(1e-200,1e-200),(-1e200,1e200)):
  center=float(mp.mpf(mean)-mp.mpf(sd)*mu/sigma)
  xs=[center,math.nextafter(center,-math.inf),math.nextafter(center,math.inf),mean,mean+sd,mean-sd]
  for x in xs:
   z=(mp.mpf(x)-mp.mpf(mean))/mp.mpf(sd)*sigma+mu
   skew=1/t if z<0 else t;v=z/skew
   ld=mp.log(2)-mp.log(t+1/t)-v*v/2-mp.log(2*mp.pi)/2-mp.log(mp.mpf(sd))+mp.log(sigma)
   small=mp.log(2)-mp.log(t+1/t)+mp.log(skew)+normal(-abs(v))[0]
   tails=pair(small,z<0)
   for ident,params in [('snorm',[mean,sd,xi]),('sged',[mean,sd,2.,xi])]:
    jobs.append((ident,0,x,True,True,params));expected.append(float(ld))
    for lower,lp in zip((True,False),tails):
     jobs.append((ident,1,x,lower,True,params));expected.append(float(lp))
     if lp>mp.log(mp.mpf('1e-300')):
      jobs.append((ident,1,x,lower,False,params));expected.append(float(mp.exp(lp)))
rows=[]
for job,a,b in zip(jobs,audit.evaluate(jobs),expected):
 e=audit.error(a,b,job[1]==1 and not job[4]);rows.append(dict(job=job,actual=a,expected=b,error=e))
(HERE/'skew-compensation.json').write_text(json.dumps(rows,indent=2)+'\n')
print('compensated skew',len(rows),'checks; worst',max(r['error'] for r in rows),flush=True)
for r in rows:
 if r['error']>1e-10:print('FAILED',r,flush=True)
assert all(r['error']<=1e-10 for r in rows)
for ident in ('snorm','sged'):
 r=next(r for r in rows if r['job'][0]==ident and r['job'][-1][-1]==1e-8 and abs(r['job'][2]-1.3236)<.001 and r['job'][1]==1 and not r['job'][3] and r['job'][4])
 _,_,x,lower,log,pars=r['job'];p=ROOT/'univariateML-cpp/include/cppunivariate/distributions'/ident/'test.cpp';title=ident+': compensated thin-side centering'
 if title not in p.read_text():p.write_text(p.read_text()+'\n// Independent 200-digit exact-input normal-member reference.\nTEST_CASE("'+title+'") {\n CHECK(cppunivariate::distributions::p'+ident+'('+','.join(map(repr,[x,*pars]))+',false,true)==doctest::Approx('+repr(r['expected'])+').epsilon(1e-10));\n}\n')
