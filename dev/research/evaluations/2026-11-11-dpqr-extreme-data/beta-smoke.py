"""Exact-input centered beta densities and rational beta-NB submodels."""
import json,math,sys
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
sys.path.insert(0,str(HERE));import audit
mp.mp.dps=1600
jobs=[];expected=[]
for a,b,x in [(16,16,.5),(1e8,1e8,.5),(1e8,2e8,1/3),(1e308,1e308,.5),(math.nextafter(1e308,0),1e308,.5),(1e292,1e308,1e-16),(1e307,3e307,.25)]:
 for xx in (x,math.nextafter(x,0),math.nextafter(x,1)):
  aa,bb,v=map(mp.mpf,(a,b,xx));base=-mp.log(mp.beta(aa,bb))
  jobs.append(('beta',0,xx,True,True,[a,b]));expected.append(float((aa-1)*mp.log(v)+(bb-1)*mp.log1p(-v)+base))
  lo,hi=-1.,1.;physical=2*xx-1
  pp=(mp.mpf(physical)-lo)/(hi-lo)
  if 0<pp<1:
   jobs.append(('nsbeta',0,physical,True,True,[a,b,lo,hi]));expected.append(float((aa-1)*mp.log(pp)+(bb-1)*mp.log1p(-pp)+base-mp.log(2)))
for a in (2.,1e8,1e308):
 for ident,pars in [('gb2',[a,1,a,1]),('fpareto',[a,1,a,1,0]),('betapr',[a,a,1])]:
  aa=mp.mpf(a);ld=-2*aa*mp.log(2)-mp.log(mp.beta(aa,aa))
  jobs.append((ident,0,1.,True,True,pars));expected.append(float(ld))
for r,a,b in [(1e8,1,1),(1,1,1e8),(1e8,1e-8,1)]:
 for x in (0.,101.,199.,1e100):
  rr,aa,bb,v=map(mp.mpf,(r,a,b,x));ld=mp.loggamma(rr+v)-mp.loggamma(rr)-mp.loggamma(v+1)+mp.log(mp.beta(aa+rr,bb+v))-mp.log(mp.beta(aa,bb))
  jobs.append(('bnbinom',0,x,True,True,[r,a,b]));expected.append(float(ld))
rows=[dict(job=j,actual=a,expected=b,error=audit.error(a,b)) for j,a,b in zip(jobs,audit.evaluate(jobs),expected)]
(HERE/'beta-smoke.json').write_text(json.dumps(rows,indent=2)+'\n')
print('beta centered smoke',len(rows),'worst',max(r['error'] for r in rows),flush=True)
for r in rows:
 if r['error']>1e-10:print('FAILED',r,flush=True)
assert all(r['error']<=1e-10 for r in rows)
for ident in ('beta','nsbeta','gb2','fpareto','betapr','bnbinom'):
 r=next(r for r in rows if r['job'][0]==ident and r['job'][5][0]==1e8)
 _,_,x,_,_,pars=r['job'];p=ROOT/'univariateML-cpp/include/cppunivariate/distributions'/ident/'test.cpp'
 title=ident+': centered large-shape density'
 if title in p.read_text():continue
 p.write_text(p.read_text()+'\n// Independent 1600-digit exact-input reference; beta-smoke.py.\nTEST_CASE("'+title+'") {\n  CHECK(cppunivariate::distributions::d'+ident+'('+','.join(map(repr,[x,*pars]))+',true)==\n        doctest::Approx('+repr(r['expected'])+').epsilon(1e-10).scale(0));\n}\n')
